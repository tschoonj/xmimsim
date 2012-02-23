#include <stdio.h>
#include <unistd.h>


int main(int argc, char *argv[]) {

	int counter = 0;
	
	setbuf(stdout,NULL);


	while (counter < 10) {
		sleep(1);
		fprintf(stdout,"stdout: line %i\n",counter);
		sleep(1);
		fprintf(stderr,"stderr: line %i\n",counter);
		counter++;
	}



	return 0;
}


