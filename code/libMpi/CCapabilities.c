#include <stdio.h>
#include <sys/sysinfo.h>
#include <unistd.h>

/**
 * This routine scrapes the meminfo "file" of the proc to get the available memory
 */
int getRamInKB(void) {
	int response = -1;
    FILE *meminfo = fopen( "/proc/meminfo", "r" );
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
//		fprintf( stderr, "loads[0]:\t%ld\n", sysInfo.loads[0] );
//		fprintf( stderr, "loads[1]:\t%ld\n", sysInfo.loads[1] );
//		fprintf( stderr, "loads[2]:\t%ld\n", sysInfo.loads[2] );
//		fprintf( stderr, "totalram:\t%ld\n", sysInfo.totalram );
//		fprintf( stderr, "freeram:\t%ld\n", sysInfo.freeram );
//		fprintf( stderr, "sharedram:\t%ld\n", sysInfo.sharedram );
//		fprintf( stderr, "bufferram:\t%ld\n", sysInfo.bufferram );
//		fprintf( stderr, "totalswap:\t%ld\n", sysInfo.totalswap );
//		fprintf( stderr, "freeswap:\t%ld\n", sysInfo.freeswap );
//		fprintf( stderr, "procs:  \t%d\n", sysInfo.procs );
//		fprintf( stderr, "totalhigh:\t%ld\n", sysInfo.totalhigh );
//		fprintf( stderr, "freehigh:\t%ld\n", sysInfo.freehigh );
//		fprintf( stderr, "mem_unit:\t%d\n", sysInfo.mem_unit );
//		fprintf( stderr, "GetRamInKB:\t%d\n", *freeRam );
	}
}

/**
 * This routine gets the number of cores
 */
void getcores( int* cores ) {
	*cores = sysconf( _SC_NPROCESSORS_ONLN );
}
