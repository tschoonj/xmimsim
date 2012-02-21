#include <stdio.h>
#include <unistd.h>


int main(int argc, char *argv[]) {

	int counter = 0;


	while (1) {
		fprintf(stdout,"stdout: line %i\n",counter);
		sleep(1);
		fprintf(stderr,"stderr: line %i\n",counter);
		sleep(1);
		counter++;
	}



	return 0;
}


