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
#include <stdlib.h>

typedef struct{
	uint32_t offset;
	uint32_t lenght;
}GME_AUDIO_FILE_TABLE;

typedef struct{
	uint16_t elements;
	uint32_t jumpTable[];
}GME_JUMP_TABLE;


typedef struct{
	uint32_t lastOIDCode;
	uint32_t firstOIDCode;
	uint32_t codeTable[];
}GME_MAIN_TABLE;

//internal structure for a loaded gme file
typedef struct{
	uint8_t *data;
	long filesize;
	uint8_t xor;

	uint32_t bookCode;
	uint32_t checksum;

	uint32_t mainTableOffset;
	GME_MAIN_TABLE *mainTable;

	uint32_t audioFileTableOffset;
	GME_AUDIO_FILE_TABLE *audioFileTable;
	uint32_t audioFileTableEntries;
}GME_FILE;

#define GME_AUDIO_FILE_TABLE_OFFSET 4
#define GME_BOOK_CODE_OFFSET 20 
#define GME_MAIN_FILE_TABLE_OFFSET 0

#define GET_UINT32(pos) (*(uint32_t*)&gme->data[pos])


//TOFIX: will fail if xor value is one of the magic characters
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

//TODO: remove; every file should be xored seperatly to be save
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
	gme->bookCode = GET_UINT32(GME_BOOK_CODE_OFFSET);
	gme->xor = getXOR(gme);
	gme->checksum = *(uint32_t*)&gme->data[gme->filesize - 4];

	gme->mainTableOffset = GET_UINT32(GME_MAIN_FILE_TABLE_OFFSET);
	gme->mainTable = &gme->data[gme->mainTableOffset];
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

GME_AUDIO_FILE_TABLE *createAudioTable(GME_FILE *gme, char *filepath,int useNumbers){
	GME_AUDIO_FILE_TABLE *aft = malloc(sizeof(GME_AUDIO_FILE_TABLE)*gme->audioFileTableEntries);
	uint32_t nextOffset = gme->audioFileTable[0].offset;
	char audioFileName[1024];
	FILE *fplist;
	int i = 0;

	if (!useNumbers) fplist = fopen(filepath, "rb");
	for (i = 0; i<gme->audioFileTableEntries; i++){
		if (useNumbers){
			sprintf(audioFileName,"%s/%04d.ogg", filepath, i);
		}else{
			fscanf(fplist, "%s", audioFileName);
		}
		aft[i].offset = nextOffset;
		aft[i].lenght = getFileSize(audioFileName);
		nextOffset += aft[i].lenght;
	}
	if (!useNumbers) fclose(fplist);
	return aft;
}

void addAudioFiles(GME_FILE *gme, GME_AUDIO_FILE_TABLE *aft, uint8_t *data, char *filepath, int useNumbers){
	char audioFileName[1024];
	FILE *fplist;
	FILE *fp;
	uint8_t *cpos = data;
	const uint8_t xorff = gme->xor ^ 0xFF;
	int i,j;

	if (!useNumbers) fplist = fopen(filepath, "rb");
	for (i = 0; i < gme->audioFileTableEntries; i++){
		if (useNumbers){
			sprintf(audioFileName,"%s/%04d.ogg", filepath, i);
		}else{
			fscanf(fplist, "%s", audioFileName);
		}
		
		fp = fopen(audioFileName, "rb");
		fread(cpos, 1, aft[i].lenght, fp);
		fclose(fp);
		for (j = 0; j < aft[i].lenght; j++){
			if (*cpos != 0 && *cpos != 0xFF && *cpos != gme->xor && *cpos != xorff) {
				*cpos = (*cpos) ^gme->xor;
			}
			cpos++;
		}
	}
	if (!useNumbers) fclose(fplist);
}


typedef struct {
	char *filename;
	uint32_t position;
	FILE *fp;
	uint32_t size;
}GME_AUDIO_FILE_POSITION;

typedef struct {
	uint8_t *data;
	GME_AUDIO_FILE_POSITION *files;
	uint32_t fileCount;
	GME_AUDIO_FILE_TABLE *aft;
	uint32_t datasize;
}GME_AUDIO_SCRIPT;

GME_AUDIO_SCRIPT *readFilelist(char *filelist, uint32_t firstOffset){
	FILE *fp;
	uint32_t filesize;
	GME_AUDIO_SCRIPT *gas;
	uint8_t *cstart, *cpos;
	int i,j;
	uint32_t nextOffset = firstOffset;
	int duplicate;

	if ((fp = fopen(filelist, "rb"))==NULL){
		fprintf(stderr, "Could not open file '%s' for reading\n", filelist);
		exit(1);
	}
	fseek(fp, 0, SEEK_END);
	filesize = ftell(fp);
	fseek(fp, 0, SEEK_SET);

	if ((gas = malloc(sizeof(GME_AUDIO_SCRIPT))) == NULL || (gas->data = malloc(filesize+1)) == NULL){
		fprintf(stderr, "Could not allocate memory\n", filelist);
		exit(1);
	}
	gas->fileCount = 0;
	gas->datasize = 0;
	if (fread(gas->data, 1, filesize, fp) != filesize){
		fprintf(stderr, "Could not read file '%s' for reading\n", filelist);
		exit(1);
	}
	gas->data[filesize] = 0;
	fclose(fp);

	//Count lines
	cpos = gas->data;
	while (*cpos!=0){
		if (*cpos == '\r' || *cpos == '\n'){
			gas->fileCount++;
			if (*cpos == '\r' && *(cpos + 1) == '\n') cpos++;
		}
		cpos++;
	}
	gas->files = malloc(sizeof(GME_AUDIO_SCRIPT)*(gas->fileCount + 1));
	
	//Fill gas->files with filenames
	cpos = gas->data;
	i = 0;
	gas->files[0].filename = gas->data;
	gas->files[0].position = 0xFFFFFFFF;
	while (*cpos != 0){
		if (*cpos == '\r' || *cpos == '\n'){
			*cpos++ = 0;
			if (*(cpos) == '\n') cpos++;
			i++;
			gas->files[i].filename = cpos;
			gas->files[i].position = 0xFFFFFFFF;
		}
		cpos++;
	}

	for (i = 0; i < gas->fileCount; i++){
		duplicate = 0;
		for (j = 0; j < i; j++){
			if (strcmp(gas->files[i].filename, gas->files[j].filename) == 0){
				duplicate = 1;
				break;
			}
		}

		if (!duplicate){
			if ((gas->files[i].fp = fopen(gas->files[i].filename, "rb")) != NULL){
				fseek(gas->files[i].fp, 0, SEEK_END);
				gas->files[i].size = ftell(gas->files[i].fp);
				fseek(gas->files[i].fp, 0, SEEK_SET);
				gas->datasize += gas->files[i].size;
			}
		}
	}
}

//Replace audio files with files from given list
void replaceAudio(char *inputfile, char *outputfile, char *filepath, int useNumbers){
	GME_FILE *gme = readFile(inputfile);
	uint32_t endofAudioData = gme->audioFileTable[gme->audioFileTableEntries - 1].offset + gme->audioFileTable[gme->audioFileTableEntries - 1].lenght;
	uint8_t *data;
	uint8_t *dataPos;
	uint32_t dataLength;
	GME_AUDIO_SCRIPT *gas;
	GME_AUDIO_FILE_TABLE *aft;
	FILE *fp;

	if (gme->filesize != endofAudioData + 4){
		fprintf(stderr, "There is data after the audio files, file would not work afterwards!\n");
		exit(1);
	}

	aft = createAudioTable(gme, filepath, useNumbers);
	dataLength = aft[gme->audioFileTableEntries - 1].offset + aft[gme->audioFileTableEntries - 1].lenght + 4;
	
	data = malloc(dataLength);
	memcpy(data, gme->data, gme->audioFileTableOffset);
	dataPos = data + gme->audioFileTableOffset;
	memcpy(dataPos, aft, sizeof(GME_AUDIO_FILE_TABLE)*gme->audioFileTableEntries);
	dataPos += sizeof(GME_AUDIO_FILE_TABLE)*gme->audioFileTableEntries;
	addAudioFiles(gme, aft, dataPos, filepath, useNumbers);
	
	dataPos = data + dataLength-4;
	*((uint32_t*)dataPos) = calculateChecksum(data, dataPos-data);

	fp = fopen(outputfile, "wb");
	fwrite(data, 1, dataLength, fp);
	fclose(fp);
}

//Exports all audio files and creates a filelist.txt with every filename
//for use by replace functionality
int exportAudioFiles(char *inputfile, char *path, int filelistOnly){
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
		fprintf(stderr, "Could not open outputfile '%s'", filename);
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

		if (!duplicate && !filelistOnly){
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

void hexDump(FILE *out, uint8_t* ptr1, uint8_t* ptr2){
	while (ptr1 < ptr2){
		fprintf(out, "%02X ", *ptr1++);
	}
}

//void markKnown(char *inputfile, char * outputfile){
//	GME_FILE *gme = readFile(inputfile);
//	FILE *fp = fopen(outputfile, "wb");
//	int i,j;
//	uint32_t jumpTableOffset;
//	uint32_t *gjt;
//	uint16_t elements;
//
//	for (i = 0; i < gme->audioFileTableEntries; i++){
//		memset(&gme->data[gme->audioFileTable[i].offset], 0, gme->audioFileTable[i].lenght);
//	}
//	memset(&gme->data[gme->audioFileTableOffset], 0, gme->audioFileTableEntries*8);
//
//	for (i = gme->mainTable->firstOIDCode; i < gme->mainTable->lastOIDCode; i++){
//		jumpTableOffset = gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode];
//		if (jumpTableOffset != 0xFFFFFFFF){
//			if (i>gme->mainTable->firstOIDCode){
//				memset(&gme->data[gjt[elements - 1]], 0, &gme->data[gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode] + 2] - &gme->data[gjt[elements - 1]]);
//			}
//			elements = *(uint16_t*)&gme->data[gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode]];
//			gjt = &gme->data[gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode] + 2];
//			for (j = 0; j < elements - 1; j++){
//				memset(&gme->data[gjt[j]], 0, &gme->data[gjt[j + 1]] - &gme->data[gjt[j]]);
//			}
//		}
//	}
//	fwrite(gme->data, 1, gme->filesize,fp);
//	fclose(fp);
//}

void printInformation(char *inputfile, FILE *out){
	GME_FILE *gme = readFile(inputfile);
	uint32_t tmp;
	int i,j;
	uint32_t jumpTableOffset;
	uint32_t *gjt;
	uint16_t elements;
	fprintf(out,"Filesize: %d\n", gme->filesize);
	fprintf(out,"File checksum: 0x%0X\n", gme->checksum);
	fprintf(out,"Calc checksum: 0x%0X\n", calculateChecksum(gme->data, gme->filesize - 4));
	fprintf(out,"XOR value: 0x%0X\n", gme->xor);
	fprintf(out,"Book Code: %d\n", gme->bookCode);

	fprintf(out,"\nMain Table:\n");
	fprintf(out,"\toffset:  %d (0x%08X)\n", gme->mainTableOffset, endianSwap(gme->mainTableOffset));
	fprintf(out,"\tlast OID Code:  %d (0x%08X)\n", gme->mainTable->lastOIDCode, endianSwap(gme->mainTable->lastOIDCode));
	fprintf(out,"\tfirst OID Code:  %d (0x%08X)\n", gme->mainTable->firstOIDCode, endianSwap(gme->mainTable->firstOIDCode));
	for (i = gme->mainTable->firstOIDCode; i < gme->mainTable->lastOIDCode; i++){
		jumpTableOffset = gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode];
		if (jumpTableOffset != 0xFFFFFFFF){
			if (i>gme->mainTable->firstOIDCode){
				fprintf(out, "\t%02d: %d: ", elements - 1, gjt[elements - 1]);
				hexDump(out, &gme->data[gjt[elements - 1]], &gme->data[gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode] + 2]);
				fprintf(out, "\n");
			}
			fprintf(out, "%02d: %d\n", i, jumpTableOffset);
			elements = *(uint16_t*)&gme->data[gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode]];
			gjt = &gme->data[gme->mainTable->codeTable[i - gme->mainTable->firstOIDCode]+2];
			for (j = 0; j < elements-1; j++){
				fprintf(out,"\t%02d: %d: ", j, gjt[j]);
				hexDump(out,&gme->data[gjt[j]], &gme->data[gjt[j + 1]]);
				fprintf(out,"\n");
			}
		}
	}
	fprintf(out,"\nAudio:\n");
	fprintf(out,"\tTable offset:  %d (0x%08X)\n", gme->audioFileTableOffset, endianSwap(gme->audioFileTableOffset));
	fprintf(out,"\tTable entries: %d\n", gme->audioFileTableEntries);
	fprintf(out,"\tFiles offset:  %d (0x%08X)\n", gme->audioFileTable[0].offset, endianSwap(gme->audioFileTable[0].offset));
	tmp = gme->audioFileTable[gme->audioFileTableEntries - 1].offset + gme->audioFileTable[gme->audioFileTableEntries - 1].lenght;
	fprintf(out,"\tFiles endoff:  %d (0x%08X)\n", tmp, endianSwap(tmp));
	fprintf(out,"\tFiles size:    %d\n", tmp - gme->audioFileTable[0].offset);
}

int main(int argc, char **argv){
	if (argc > 2){
		switch (argv[1][0]){
		case 'X':
		case 'x':
			if (argc == 3){
				exportAudioFiles(argv[2], "", 0);
				return 1;
			}
			else if (argc == 4){
				exportAudioFiles(argv[3], argv[2], 0);
				return 1;
			}
			break;
		case 'I':
		case 'i':
			if (argc == 3){
				printInformation(argv[2], stdout);
				return 1;
			}
			else if (argc == 4){
				FILE *fp = fopen(argv[2], "wb");
				printInformation(argv[3],fp);
				fclose(fp);
				return 1;
			}
			break;
		case 'R':
		case 'r':
			if (argc == 5){
				replaceAudio(argv[4], argv[3], argv[2], 0);
				return 1;
			}
			break;
		case 'N':
		case 'n':
			if (argc == 5){
				replaceAudio(argv[4], argv[3], argv[2], 1);
				return 1;
			}
			break;
		//case 'M':
		//case 'm':
		//	if (argc == 4){
		//		markKnown(argv[3], argv[2]);
		//		return 1;
		//	}
		//	break;
		}
	}

	printf("GME Library\n");
	printf("usage: libtiptoi <mode> [options] <inputfile>\n");
	printf("modes:\n");
	printf("\tx [path] - Extract audio files to [path]\n");
	printf("\tl <filelist> - create audio list\n");
	printf("\ti [outputfile] - print information\n");
	printf("\tr <filelist> <outputfile> - replace audio\n");
	printf("\tn <audiopath> <outputfile> - replace audio with numbers\n");
	//printf("\tp <outputfile> - replace known bytes with 0x00 in gme\n");
	return 0; 
}

//r C:\Projects\tiptoi\ogg\filelist.txt "C:\Projects\tiptoi\Rekorde im Tierreich_mod.gme" "C:\Projects\tiptoi\Rekorde im Tierreich.gme"
//x C:\Projects\tiptoi\ogg\ "C:\Projects\tiptoi\Rekorde im Tierreich.gme"

//n C:\Projects\tiptoi\ogg\filelist.txt "C:\Projects\tiptoi\WWW Bauernhof_mod.gme" "C:\Projects\tiptoi\WWW Bauernhof.gme"
//x C:\Projects\tiptoi\ogg\ "C:\Projects\tiptoi\WWW Bauernhof.gme"
