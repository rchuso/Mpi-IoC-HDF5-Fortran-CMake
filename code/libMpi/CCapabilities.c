#include <stdio.h>
#include <sys/sysinfo.h>
#include <unistd.h>

// Written by Rand Huso

/**
 * This routine scrapes the meminfo "file" of the proc to get the available memory
 */
int getRamInKB(void) {
	int response = -1;
    FILE *meminfo = fopen( "/proc/meminfo", "r" ); // assuming a linux machine - and most clusters are linux anyway
    if( meminfo != NULL ) {
		char line[256];
		int value;
		while( fgets(line, sizeof(line), meminfo )) {
			if( sscanf( line, "MemAvailable: %d kB", &value ) == 1 ) {
				response = value;
				break;
			}
		}
		fclose(meminfo);
    }
    return response;
}

/**
 * This routine sets and returns the amount of free and total memory in the system
 */
void getmem( unsigned long*freeRam, unsigned long*totalRam ) {
	struct sysinfo sysInfo;
	*freeRam = 0l;
	*totalRam = 0l;
	if( 0 == sysinfo( &sysInfo ) ) {
		*totalRam = sysInfo.mem_unit * sysInfo.totalram / (1024 * 1024);
		*freeRam = getRamInKB() / 1024;
	}
}

/**
 * This routine gets the number of cores
 */
void getcores( int* cores ) {
	*cores = sysconf( _SC_NPROCESSORS_ONLN );
}
