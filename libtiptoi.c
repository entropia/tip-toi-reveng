/* TipToi Datafile Tool
Copyright(c) 2013 Michael Wolf

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files(the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions :

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include <stdio.h>
#include <stdint.h>
#include <string.h>

typedef struct{
	uint32_t offset;
	uint32_t lenght;
}GME_AUDIO_FILE_TABLE;

//internal structure for a loaded gme file
typedef struct{
	uint8_t *data;
	long filesize;
	uint32_t audioFileTableOffset;
	GME_AUDIO_FILE_TABLE *audioFileTable;
	uint32_t audioFileTableEntries;
	uint8_t xor;
	uint32_t checksum;
}GME_FILE;

#define GME_AUDIO_FILE_TABLE_OFFSET 4

#define GET_UINT32(pos) (*(uint32_t*)&gme->data[pos])

uint8_t getXOR(GME_FILE *gme){
	uint8_t magic[2][5] = { "OggS", "RIFF" };
	unsigned char *file = (char*)&gme->data[gme->audioFileTable[0].offset];
	uint8_t xor;
	int m,c;
	for (m = 0; m < 2; m++){
		xor = file[0] ^ magic[m][0];
		if ((file[1] ^ xor == magic[m][1]) && (file[2] ^ xor == magic[m][2]) && (file[3] ^ xor == magic[m][3])){
			return xor;
		}
	}
	return 0;
}

void xorAudio(GME_FILE *gme){
	uint8_t *pos = &gme->data[gme->audioFileTable[0].offset];
	uint8_t *end = &gme->data[gme->audioFileTable[gme->audioFileTableEntries-1].offset + gme->audioFileTable[gme->audioFileTableEntries-1].lenght];
	const uint8_t xorff = gme->xor ^ 0xFF;
	while (pos <= end){
		if (*pos != 0 && *pos != 0xFF && *pos != gme->xor && *pos != xorff) {
			*pos = *pos ^gme->xor;
		}
		pos++;
	}
}

uint32_t calculateChecksum(uint8_t *data, uint32_t size){
	uint32_t checksum = 0;
	int i;
	for (i = 0; i < size; i++){
		checksum += *data++;
	}
	return checksum;
}

GME_FILE *readFile(char *filename){
	FILE *fp;
	GME_FILE *gme;
	uint32_t checksumData = 0;
	uint32_t checksumFile = 0;

	if ((fp = fopen(filename, "rb")) == NULL) return NULL;

	if ((gme = malloc(sizeof(GME_FILE))) == NULL){
		fprintf(stderr,"Error could not allocate gme file structure\n");
		fclose(fp);
		return NULL;
	}
	
	fseek(fp, 0, SEEK_END);
	gme->filesize=ftell(fp);
	fseek(fp, 0, SEEK_SET);

	if ((gme->data = malloc(gme->filesize)) == NULL){
		fprintf(stderr, "Cold not allocate memory for filedata\n");
		fclose(fp);
		free(gme);
		return NULL;
	}
	if (fread(gme->data, 1, gme->filesize, fp) != gme->filesize){
		fprintf(stderr, "Cold not read filedata\n");
		fclose(fp);
		free(gme->data);
		free(gme);
		return NULL;
	}
	fclose(fp);

	gme->audioFileTableOffset = GET_UINT32(GME_AUDIO_FILE_TABLE_OFFSET);
	gme->audioFileTable = &gme->data[gme->audioFileTableOffset];
	gme->audioFileTableEntries = (gme->audioFileTable[0].offset - GET_UINT32(GME_AUDIO_FILE_TABLE_OFFSET)) / 8;
	gme->xor = getXOR(gme);
	gme->checksum = *(uint32_t*)&gme->data[gme->filesize - 4];
	return gme;
}

void freeGme(GME_FILE *gme){
	free(gme->data);
	free(gme);
}


uint32_t endianSwap(uint32_t value){
	return

		(value & 0x000000FFu) << 24 |
		(value & 0x0000FF00u) << 8 |
		(value & 0x00FF0000u) >> 8 |
		(value & 0xFF000000u) >> 24;
}

long getFileSize(char *filename){
	FILE *fp;
	long size;

	if ((fp = fopen(filename, "rb")) == NULL) return 0;
	fseek(fp, 0, SEEK_END);
	size = ftell(fp);
	fclose(fp);
	return size;
}

GME_AUDIO_FILE_TABLE *createAudioTable(GME_FILE *gme, char *filelist){
	GME_AUDIO_FILE_TABLE *aft = malloc(sizeof(GME_AUDIO_FILE_TABLE)*gme->audioFileTableEntries);
	uint32_t nextOffset = gme->audioFileTable[0].offset;
	char **audioFileName = malloc(gme->audioFileTableEntries*1024);
	FILE *fplist = fopen(filelist, "rb");
	int i = 0;
	for (i = 0; i<gme->audioFileTableEntries; i++){
		fscanf(fplist, "%s", audioFileName[i]);
		aft[i].offset = nextOffset;
		aft[i].lenght = getFileSize(audioFileName[i]);
		nextOffset += aft[i].lenght;
	}
	fclose(fplist);
	return aft;
}

void addAudioFiles(GME_FILE *gme, GME_AUDIO_FILE_TABLE *aft, uint8_t *data, char *filelist){
	char *audioFileName[1024];
	FILE *fplist = fopen(filelist, "rb");
	FILE *fp;
	uint8_t *cpos = data;
	const uint8_t xorff = gme->xor ^ 0xFF;

	int i = 0;
	for (i = 0; i < gme->audioFileTableEntries; i++){
		fscanf(fplist, "%s", audioFileName);
		fp = fopen(audioFileName, "rb");
		fread(cpos, 1, aft[i].lenght, fp);
		fclose(fp);
		for (i = 0; i < aft[i].lenght; i++){
			if (*cpos != 0 && *cpos != 0xFF && *cpos != gme->xor && *cpos != xorff) {
				*cpos = *cpos ^gme->xor;
			}
			cpos++;
		}
		cpos += aft[i].lenght;
	}
	fclose(fplist);
}

void xorData(GME_FILE *gme){
	uint8_t *pos = &gme->data[gme->audioFileTable[0].offset];
	uint8_t *end = &gme->data[gme->audioFileTable[gme->audioFileTableEntries - 1].offset + gme->audioFileTable[gme->audioFileTableEntries - 1].lenght];
	const uint8_t xorff = gme->xor ^ 0xFF;
	while (pos <= end){
		if (*pos != 0 && *pos != 0xFF && *pos != gme->xor && *pos != xorff) {
			*pos = *pos ^gme->xor;
		}
		pos++;
	}
}

//Replace audio files ib given list
//NOT JET TESTED AND ITS CODE FROM 2AM! :)
void replaceAudio(char *inputfile, char *outputfile, char *filelist){
	GME_FILE *gme = readFile(inputfile);
	uint32_t endofAudioData = gme->audioFileTable[gme->audioFileTableEntries - 1].offset + gme->audioFileTable[gme->audioFileTableEntries - 1].lenght;
	uint32_t *data;
	uint32_t *dataPos;
	uint32_t dataLength;
	GME_AUDIO_FILE_TABLE *aft;
	FILE *fp;

	if (gme->filesize != endofAudioData + 4){
		fprintf(stderr, "There is data after the audio files, file would not work afterwards!\n");
		exit(1);
	}

	aft = createAudioTable(gme, filelist);
	dataLength = aft[gme->audioFileTableEntries - 1].offset + aft[gme->audioFileTableEntries - 1].lenght + 4;
	
	data = malloc(dataLength);
	memcpy(data, gme->data, gme->audioFileTableOffset);
	dataPos = data + gme->audioFileTableOffset;
	memcpy(dataPos, aft, sizeof(GME_AUDIO_FILE_TABLE)*gme->audioFileTableEntries);
	dataPos += sizeof(GME_AUDIO_FILE_TABLE)*gme->audioFileTableEntries;
	addAudioFiles(gme, aft, data, filelist);
	
	dataPos = data + dataLength - 4;
	*dataPos = calculateChecksum(data, dataLength - 4);

	fp = fopen(outputfile, "wb");
	fwrite(data, 1, dataLength, fp);
	fclose(fp);
}

//Exports all audio files and creates a filelist.txt with every filename
//for use by replace functionality
int exportAudioFiles(char *inputfile, char *path){
	GME_FILE *gme = readFile(inputfile);
	char filename[1024];
	uint8_t *file;
	int i, j, duplicate;
	FILE *fp;
	FILE *fplist;

//TODO: XOR every Audiofile separatly? there could be holes or overlaps with other data (not found jet in samples)
	xorAudio(gme);

	sprintf(filename, "%sfilelist.txt", path);
	if ((fplist = fopen(filename, "wb")) == NULL){
		fprintf(stderr, "Could not open outputfile '%s'");
		exit(0);
	}
	for (i = 0; i < gme->audioFileTableEntries; i++){
		duplicate = 0;
		/*
		check if we allready exported a file with current offset
		needed for "Rekorde im Tierreich.gme" and possibly others where there are multible entries
		in the table to the same audiofile
		brute force check is slow but ok for some k entries
		*/
		for (j = 0; j < i; j++){
			if (gme->audioFileTable[i].offset == gme->audioFileTable[j].offset){
				duplicate = 1;
				break;
			}
		}
		file = &gme->data[gme->audioFileTable[i].offset];
		if (*file == 'O'){
			sprintf(filename, "%s%04d.ogg", path, duplicate ? j : i);
		}
		else if (*file == 'R'){
			sprintf(filename, "%s%04d.wav", path, duplicate ? j : i);
		}
		else{
			sprintf(filename, "%s%04d.raw", path, duplicate ? j : i);
		}
		fprintf(fplist, "%s\n", filename);

		if (!duplicate){
			if ((fp = fopen(filename, "wb")) == NULL){
				fprintf(stderr, "Could not open file '%s' for writing\n", filename);
				continue;
			}
			fwrite(file, 1, gme->audioFileTable[i].lenght, fp);
			fclose(fp);
		}
	}
	fclose(fplist);
	freeGme(gme);
}

void printInformation(char *inputfile){
	GME_FILE *gme = readFile(inputfile);
	uint32_t tmp;

	printf("Filesize: %d\n", gme->filesize);
	printf("File checksum: 0x%0X\n", gme->checksum);
	printf("Calc checksum: 0x%0X\n", calculateChecksum(gme->data, gme->filesize - 4));
	printf("XOR value: 0x%0X\n", gme->xor);

	printf("\nAudio:\n");
	printf("\tTable offset:  %d (0x%08X)\n", gme->audioFileTableOffset, endianSwap(gme->audioFileTableOffset));
	printf("\tTable entries: %d\n", gme->audioFileTableEntries);
	printf("\tFiles offset:  %d (0x%08X)\n", gme->audioFileTable[0].offset, endianSwap(gme->audioFileTable[0].offset));
	tmp = gme->audioFileTable[gme->audioFileTableEntries - 1].offset + gme->audioFileTable[gme->audioFileTableEntries - 1].lenght;
	printf("\tFiles endoff:  %d (0x%08X)\n", tmp, endianSwap(tmp));
	printf("\tFiles size:    %d\n", tmp - gme->audioFileTable[0].offset);
}

int main(int argc, char **argv){
	if (argc > 2){
		switch (argv[1][0]){
		case 'X':
		case 'x':
			if (argc == 3){
				exportAudioFiles(argv[2], "");
				return 1;
			}else if (argc == 4){
				exportAudioFiles(argv[3], argv[2]);
				return 1;
			}
			break;
		case 'I':
		case 'i':
			if (argc == 3){
				printInformation(argv[2]);
				return 1;
			}
			break;
		case 'R':
		case 'r':
			if (argc == 5){
				replaceAudio(argv[4], argv[3], argv[2]);
				return 1;
			}
			break;
		}
	}

	printf("GME Library\n");
	printf("usage: libtiptoi <mode> [options] <inputfile>\n");
	printf("modes:\n");
	printf("\tx [path] - Extract audio files to [path]\n");
	printf("\ti - print information\n");
	printf("\tr <filelist> <outputfile> - replace audio\n");

	return 0; 
}

//r C:\Projects\tiptoi\ogg\filelist.txt "C:\Projects\tiptoi\Rekorde im Tierreich_mod.gme" "C:\Projects\tiptoi\Rekorde im Tierreich.gme"
//x C:\Projects\tiptoi\ogg\ "C:\Projects\tiptoi\Rekorde im Tierreich.gme"