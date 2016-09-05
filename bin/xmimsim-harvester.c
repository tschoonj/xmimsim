/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <syslog.h>
#include <fcntl.h>
#include <sys/resource.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "xmi_random_dev.h"
#include <sys/resource.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <glib.h>
#include <glib/gstdio.h>


//#define LOCKFILE "/var/run/xmi_harvester.pid"
#define LOCKMODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)


struct harvester {
	char fifoslave[100];
	int nseeds;
};


void *harvest_thread(void *sh) {
	struct harvester *shl = (struct harvester *) sh;
	unsigned long int *seeds;
	int fifofd2;
	int i;

	if ((fifofd2 = open(shl->fifoslave,O_WRONLY)) == -1) {
		syslog(LOG_ERR,"Could not open named link %s: %s",shl->fifoslave,strerror(errno));
		exit(1);
	}

	seeds = (unsigned long int *) malloc(sizeof(unsigned long int)*shl->nseeds);
	for (i = 0 ; i < shl->nseeds ; i++) {
		if (xmi_get_random_number_dev(seeds+i) != 1) {
			syslog(LOG_ERR,"xmi_get_random_number error");
			exit(1);
		}
#ifdef DEBUG
		syslog(LOG_INFO,"Daemon -> seeds: %lu",seeds[i]);
#endif
	}
	if (write(fifofd2,seeds,sizeof(unsigned long int)*shl->nseeds) != sizeof(unsigned long int)*shl->nseeds) {
		syslog(LOG_ERR,"Error writing to %s: %s",shl->fifoslave,strerror(errno));
		exit(1);
	}
	//cleanup
	free(seeds);
	close(fifofd2);
	free(sh);

	return NULL;
}


void daemonize(const char *cmd) {
	int i , fd0, fd1, fd2;
	pid_t pid;
	struct rlimit rl;
	struct sigaction sa;

	umask(0);

	if (getrlimit(RLIMIT_NOFILE, &rl) < 0) {
		fprintf(stderr,"%s: can't get file limit\n",cmd);
		exit(1);
	}

	if ((pid = fork()) < 0) {
		fprintf(stderr,"%s: can't fork\n",cmd);
		exit(1);
	}
	else if (pid != 0)
		exit(0);
	setsid();

	sa.sa_handler = SIG_IGN;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = 0;
	if (sigaction(SIGHUP, &sa, NULL) < 0) {
		fprintf(stderr,"%s: can't ignore SIGHUP\n",cmd);
		exit(1);
	}
	if ((pid = fork()) < 0) {
		fprintf(stderr,"%s: can't fork\n",cmd);
		exit(1);
	}
	else if (pid != 0)
		exit(0);

	if (chdir("/") < 0) {
		fprintf(stderr,"%s: can't change directory to /\n",cmd);
		exit(1);
	}

	if (rl.rlim_max == RLIM_INFINITY)
		rl.rlim_max = 1024;
	for (i=0 ; i < rl.rlim_max ; i++) {
		close(i);
	}

	fd0 = open("/dev/null",O_RDWR);
	fd1 = dup(0);
	fd2 = dup(0);

	openlog(cmd, LOG_CONS | LOG_PID, LOG_DAEMON);
	if (fd0 != 0  || fd1 != 1 || fd2 != 2) {
		syslog(LOG_ERR,"unexpected file descriptors %d %d %d",fd0,fd1,fd2);
		exit(1);
	}

	setlogmask(LOG_UPTO(LOG_DEBUG));

}


int lockfile(int fd) {
	struct flock fl = {.l_type = F_WRLCK, .l_start = 0, .l_whence = SEEK_SET, .l_len = 0};

	return fcntl(fd,F_SETLK, &fl);
}



int already_running(void) {
	int fd;
	char buf[16];

	fd = open(LOCKFILE, O_RDWR|O_CREAT, LOCKMODE);
	if (fd < 0) {
		syslog(LOG_ERR,"can't open %s: %s",LOCKFILE,strerror(errno));
		exit(1);
	}
	if (lockfile(fd) < 0) {
		if (errno == EACCES || errno == EAGAIN) {
			close(fd);
			return 1;
		}
		syslog(LOG_ERR, "can't lock %s: %s",LOCKFILE, strerror(errno));
		exit(1);
	}
	ftruncate(fd,0);
	sprintf(buf,"%ld",(long) getpid());
	write(fd,buf,strlen(buf)+1);
	return 0;
}

void sigterm(int signo) {
	syslog(LOG_INFO, "got signal %i: exiting", signo);
	if (xmi_end_random_acquisition_dev() != 1) {
		syslog(LOG_ERR,"xmi_end_random_acquisition_dev error");
		_exit(1);
	}
	unlink(LOCKFILE);
	unlink(FIFOMASTER);
	_exit(0);
}

void sighup(int signo) {
	syslog(LOG_INFO, "got SIGHUP; doing nothing really");
}


int main (int argc, char *argv[]) {
	char *cmd;
	struct sigaction sa;
	int fifofd;
	long int pidslave[3];
//	char fifoslave[512];
	struct harvester *sh;
	pthread_t *ht;
	pthread_attr_t ha;
	GError *error = NULL;
	GOptionContext *context;
	static int use_daemonize = 1;
	static GOptionEntry entries[] = {
		{"disable-daemonize", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &use_daemonize, "do not daemonize", NULL},
		{NULL}
	};


	context = g_option_context_new ("launch seed harvester");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-harvester: a daemon that gathers seeds from /dev/random...");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		exit (1);
	}

	if ((cmd = strrchr(argv[0], '/')) == NULL)
		cmd = argv[0];
	else
		cmd++;

	if (use_daemonize)
		daemonize(cmd);
	else {
		openlog(cmd, LOG_CONS | LOG_PID, LOG_DAEMON);
		setlogmask(LOG_UPTO(LOG_DEBUG));
	}

	if (already_running()) {
		syslog(LOG_ERR, "daemon already running");
		exit(1);
	}

	sa.sa_handler = sigterm;
	sigfillset(&sa.sa_mask);
	//sigaddset(&sa.sa_mask, SIGTERM);
	sa.sa_flags=0;
	if (sigaction(SIGTERM, &sa, NULL) < 0) {
		syslog(LOG_ERR, "can't catch SIGTERM: %s", strerror(errno));
		exit(1);
	}
	if (sigaction(SIGSEGV, &sa, NULL) < 0) {
		syslog(LOG_ERR, "can't catch SIGSEGV: %s", strerror(errno));
		exit(1);
	}
	if (sigaction(SIGBUS, &sa, NULL) < 0) {
		syslog(LOG_ERR, "can't catch SIGBUS: %s", strerror(errno));
		exit(1);
	}
	if (sigaction(SIGILL, &sa, NULL) < 0) {
		syslog(LOG_ERR, "can't catch SIGILL: %s", strerror(errno));
		exit(1);
	}
	if (sigaction(SIGQUIT, &sa, NULL) < 0) {
		syslog(LOG_ERR, "can't catch SIGQUIT: %s", strerror(errno));
		exit(1);
	}

	sa.sa_handler = sighup;
	sigemptyset(&sa.sa_mask);
	sigaddset(&sa.sa_mask, SIGHUP);
	sa.sa_flags=0;
	if (sigaction(SIGHUP, &sa, NULL) < 0) {
		syslog(LOG_ERR, "can't catch SIGHUP: %s", strerror(errno));
		exit(1);
	}


	if (xmi_start_random_acquisition_dev() != 1) {
		syslog(LOG_ERR,"xmi_start_random_acquisition_dev error");
		exit(1);
	}


	syslog(LOG_INFO,"daemon running succesfully");

	pthread_attr_init(&ha);
	pthread_attr_setdetachstate(&ha,PTHREAD_CREATE_DETACHED);


	while (1) {
		//create fifo
		if (mkfifo(FIFOMASTER, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH) == -1) {
			syslog(LOG_ERR,"Could not create named link" FIFOMASTER ": %s",strerror(errno));
			exit(1);
		}
		if ((fifofd = open(FIFOMASTER,O_RDONLY)) == -1) {
			syslog(LOG_ERR,"Could not open named link" FIFOMASTER ": %s",strerror(errno));
			exit(1);
		}

		//read pid from slave
		if (read(fifofd,pidslave,3*sizeof(long int)) != 3*sizeof(long int)) {
			syslog(LOG_ERR,"Error reading from " FIFOMASTER ": %s",strerror(errno));
			exit(1);
		}
		close(fifofd);
		unlink(FIFOMASTER);
#ifdef DEBUG
		syslog(LOG_INFO,"Daemon -> pid: %li  counter: %li number: %li",pidslave[0],pidslave[1],pidslave[2]);
#endif
		//allocate necessary variables
		sh = (struct harvester *) malloc(sizeof(struct harvester));
		ht = (pthread_t *) malloc(sizeof(pthread_t));

		//create slave fifo
		sprintf(sh->fifoslave, FIFOSLAVE "%li.%li",pidslave[0],pidslave[1]);
		sh->nseeds = pidslave[2];

		//start thread
		pthread_create(ht,&ha,harvest_thread,sh);




	}


	return 0;
}


