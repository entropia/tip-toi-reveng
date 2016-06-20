
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define DUMP_BLOCKS 0

char *gme;

uint16_t start_oid;
uint16_t end_oid;
uint16_t count_games;
uint16_t count_media;
uint32_t file_size;

uint32_t end_game;
uint32_t unknown_table;
uint32_t media_table;

void prepare_tests(FILE * file)
{
    fseek(file, 0, SEEK_END);
    file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint32_t p_script_table;
    fread(&p_script_table, sizeof(p_script_table), 1, file);

    fseek(file, p_script_table, SEEK_SET);
    fread(&end_oid, sizeof(end_oid), 1, file);
    fseek(file, 2, SEEK_CUR);
    fread(&start_oid, sizeof(start_oid), 1, file);

    fseek(file, 4, SEEK_SET);
    uint32_t p_media_table;
    fread(&p_media_table, sizeof(p_media_table), 1, file);
    media_table = p_media_table;

    fseek(file, 0x18, SEEK_SET);
    uint32_t p_unknown_table;
    fread(&p_unknown_table, sizeof(p_unknown_table), 1, file);
    unknown_table = p_unknown_table;
    end_game = p_unknown_table;

    uint32_t p_media_start, v_media_length, p_media_next;
    do {
	fseek(file,
	      p_media_table + count_media++ * 2 * sizeof(p_media_start),
	      SEEK_SET);
	fread(&p_media_start, sizeof(p_media_start), 1, file);
	fread(&v_media_length, sizeof(v_media_length), 1, file);
	fread(&p_media_next, sizeof(p_media_next), 1, file);
    }
    while (p_media_start + v_media_length == p_media_next);
}

int
test_list_16(FILE * file, uint32_t start, uint16_t v_begin, uint16_t v_end)
{
    int result = 0;

    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);

    uint16_t i;
    for (i = 0; i < count; i++) {
	uint16_t value;
	fseek(file, start + 2 + i * sizeof(value), SEEK_SET);
	fread(&value, sizeof(value), 1, file);
	if ((value < v_begin) || (value > v_end)) {
	    result++;
	}
    }
    if (count == 0)
	result = -1;

    return result;
}

int
test_list_32(FILE * file, uint32_t start, uint32_t v_begin, uint32_t v_end)
{
    int result = 0;

    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);

    uint16_t i;
    for (i = 0; i < count; i++) {
	uint32_t value;
	fseek(file, start + 2 + i * sizeof(value), SEEK_SET);
	fread(&value, sizeof(value), 1, file);
	if ((value < v_begin) || (value > v_end)) {
	    result++;
	}
    }
    if (count == 0)
	result = -1;

    return result;
}

int test_list_oid(FILE * file, uint32_t start)
{
    return test_list_16(file, start, start_oid, end_oid);
}

int test_list_gid(FILE * file, uint32_t start)
{
    return test_list_16(file, start, 0, count_games);
}

int test_list_media(FILE * file, uint32_t start)
{
    return test_list_16(file, start, 0, count_media);
}

void save_block(char *name, uint32_t start, uint32_t size)
{

    char out_name[2024];

    snprintf(out_name, 2020, "%s_%s", gme, name);

    FILE *out = fopen(out_name, "wb");

    FILE *in = fopen(gme, "rb");
    fseek(in, start, SEEK_SET);

    unsigned char buffer = 0;

    int i;
    for (i = 0; i < size; i++) {
	fread(&buffer, sizeof(buffer), 1, in);
	fwrite(&buffer, sizeof(buffer), 1, out);

    }

    fclose(in);
    fclose(out);
}

void parse_list_16(FILE * file, uint32_t start)
{
    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);
    printf("List (0x%X, %d) = [ ", start, count);
    int i = 0;
    for (i = 0; i < count; i++) {
	fflush(stdout);
	uint16_t id;
	fseek(file, start + 2 + i * 2, SEEK_SET);
	fread(&id, sizeof(id), 1, file);
	printf("%3d, ", id);
    }
    printf(" ]\n");
}

void parse_playlist(FILE * file, uint32_t start)
{
    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);
    printf("\t\t\t\tPlaylist (0x%X, %d) = [ ", start, count);
    int i = 0;
    for (i = 0; i < count; i++) {
	fflush(stdout);
	uint16_t id;
	fseek(file, start + 2 + i * 2, SEEK_SET);
	fread(&id, sizeof(id), 1, file);
	printf("%3d, ", id);
    }
    printf(" ]\n");
}

void parse_oidlist(FILE * file, uint32_t start)
{
    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);
    printf("\t\t\t\tOIDlist (0x%X, %d) = [ ", start, count);
    int i = 0;
    for (i = 0; i < count; i++) {
	fflush(stdout);
	uint16_t id;
	fseek(file, start + 2 + i * 2, SEEK_SET);
	fread(&id, sizeof(id), 1, file);
	printf("%3d, ", id);
    }
    printf(" ]\n");
}

void parse_gameidlist(FILE * file, uint32_t start)
{
    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);
    printf("\t\t\t\tGamelist (0x%X, %d) = [ ", start, count);
    int i = 0;
    for (i = 0; i < count; i++) {
	fflush(stdout);
	uint16_t id;
	fseek(file, start + 2 + i * 2, SEEK_SET);
	fread(&id, sizeof(id), 1, file);
	printf("%3d, ", id);
    }
    printf(" ]\n");
}

void
parse_playlistpointer(FILE * file, uint32_t start, uint32_t pbegin,
		      uint32_t pend)
{
    uint16_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);

    printf("\t\t\tPlaylistpointer (0x%X, %d)\n", start, count);
    int i = 0;
    int playerror = 0;
    for (i = 0; i < count; i++) {
	uint32_t pointer;
	fseek(file, start + 2 + i * 4, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	if ((pointer < pbegin) || (pointer > pend)) {
	    printf("\t\t\t\tPlaylist (0x%X) = PlayError!\n ", pointer);
	    playerror = 1;
	} else {
	    parse_playlist(file, pointer);
	}
    }
    if (playerror == 1) {
	parse_playlist(file, start);
    }


}

void
print_block(FILE * file, uint32_t start, uint32_t end, uint32_t bstart,
	    uint32_t bend)
{
    uint32_t blocks[50001];
    blocks[0] = start;
    int bi = 1;
    int i = 0;
    do {
	i++;
	fseek(file, start + i, SEEK_SET);
	uint32_t testp;
	fread(&testp, sizeof(testp), 1, file);
	if ((testp >= start) && (testp < end)) {
	    blocks[bi++] = testp;
	}
    }
    while ((bi < 50000) && (i < (end - start - 3)));
    if (bi < 50000) {
	blocks[bi++] = end;
	// sortieren
	int z = 0;
	for (i = 0; i < bi; i++) {
	    int j;
	    for (j = i + 1; j < bi; j++) {
		if (blocks[i] > blocks[j]) {
		    uint32_t tmp = blocks[i];
		    blocks[i] = blocks[j];
		    blocks[j] = tmp;
		}
		if (blocks[i] == blocks[j] && (blocks[i] != 0xffffffff)) {
		    blocks[j] = 0xffffffff;
		    z++;
		}
	    }
	}
	bi -= z;
	// ausgeben
	for (i = 0; i < (bi - 1); i++) {
	    printf("0x%X B%02d %06d\n\t", blocks[i], i,
		   blocks[i + 1] - blocks[i]);
	    uint32_t j;
	    int p = 0;
	    for (j = blocks[i]; j < blocks[i + 1]; j++) {
		fseek(file, j, SEEK_SET);
		uint8_t val;
		fread(&val, sizeof(val), 1, file);
		printf("%02X ", val);
		if (p == 7) {
		    printf(" ");
		}
		if (p++ == 15) {
		    p = 0;
		    printf("\n\t");
		}
	    }

	    printf("\n\t\t");
	    int coun = 0;
	    uint32_t last = 0;
	    int k = 0;
	    for (j = blocks[i]; j < blocks[i + 1]; j++) {
		fseek(file, j, SEEK_SET);
		uint32_t point;
		fread(&point, sizeof(point), 1, file);
		if ((point >= bstart) && (point < bend)) {
		    j += 3;
		    if (last < point) {
			coun++;
			last = point;
		    } else {
			printf("LB%02d I%02d L%02d \n\t\t", i, k++, coun);
			coun = 1;
			last = 0;
		    }
		} else {
		    if (coun > 0) {
			printf("LB%02d I%02d L%02d \n\t\t", i, k++, coun);
			coun = 0;
			last = 0;
		    }
		}
	    }
	    if (coun > 0) {
		printf("LB%02d I%02d L%02d \n\t\t", i, k++, coun);
		coun = 0;
		last = 0;
	    }
	    printf("\n");
	}
    }
    printf("EndSub\n");
}

void
parse_subgame(FILE * file, uint32_t start, uint32_t end, uint16_t type)
{

    uint16_t index = 20;
    fseek(file, start + index, SEEK_SET);
    int i;
    for (i = 0; i < 3; i++) {
	uint16_t count;
	fread(&(count), sizeof(count), 1, file);
	printf("\t\tOID List (%d, %d) = [ ", i, count);
	int j;
	for (j = 0; j < count; j++) {
	    uint16_t oid;
	    fread(&oid, sizeof(oid), 1, file);
	    printf("%d, ", oid);
	}
	printf(" ]\n");
    }
    int pindex = ftell(file) - start;
    for (i = 0; i < 9; i++) {

	uint32_t pointer;
	fseek(file, start + pindex + i * 4, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	if (test_list_32(file, pointer, 0, file_size) > 0) {
	    printf("No Playlist pointer (%08X)\n", pointer);
	} else {
	    parse_playlistpointer(file, pointer, 0, file_size);
	}
    }

    uint16_t tmp;
    fseek(file, start + index, SEEK_SET);
    uint16_t count[3];
    fread(&(tmp), sizeof(tmp), 1, file);
    count[0] = tmp;

    index += 2 + count[0] * 2;
    fseek(file, start + index, SEEK_SET);
    fread(&(tmp), sizeof(tmp), 1, file);
    count[1] = tmp;

    index += 2 + count[1] * 2;
    fseek(file, start + index, SEEK_SET);
    fread(&(tmp), sizeof(tmp), 1, file);
    count[2] = tmp;
    index += 2 + count[2] * 2;

    int testlen =
	20 + 2 + 2 * count[0] + 2 + 2 * count[1] + 2 + 2 * count[2] +
	4 * 9;
    if (testlen != (end - start)) {
	printf(" 20 + 2 + %d + 2 + %d + 2 + %d + %d = %d != %d\n",
	       count[0] * 2, count[1] * 2, count[2] * 2, 9 * 4, testlen,
	       end - start);
	printf("SubType%02d Error2!\n", type);
    }

    if (DUMP_BLOCKS) {
	if (start < end) {
	    print_block(file, start, end, start, end);
	} else {
	    print_block(file, start, start + testlen, start,
			start + testlen);
	}
//      hinten immer 9 * 32 bit
    }
}


void parse_game(FILE * file, int id, uint32_t start, uint32_t end)
{

    fseek(file, start, SEEK_SET);

    uint32_t index = 0;
    uint16_t type = 0;
    fread(&type, sizeof(type), 1, file);
    printf("\tType:\t%d\n", type);
    if (type == 253) {
	print_block(file, start, end, start, end);
	uint16_t count;
	fseek(file, start + 2, SEEK_SET);
	fread(&count, sizeof(count), 1, file);
	index = 4;

	int i;
	for (i = 0; i < count; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    parse_list_16(file, pointer);
	}

	printf("Typ%03d (%d)PLY ", type, count);
	for (i = 0; i < count; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_media(file, pointer);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }
	}

	printf("\n");
	index = 4;

	printf("Typ%03d (%d)OID ", type, count);

	for (i = 0; i < count; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_oid(file, pointer);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }
	}

	printf("\n");
	index = 4;

	printf("Typ%03d (%d)GID ", type, count);

	for (i = 0; i < count; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_gid(file, pointer);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }
	}
	printf("\n");
	return;
    }

    uint16_t aa;
    fread(&aa, sizeof(aa), 1, file);

    uint16_t ignore1;
    fread(&ignore1, sizeof(ignore1), 1, file);

    uint16_t bb;
    fread(&bb, sizeof(bb), 1, file);

    int playscripts = 5;
    int ps2 = 10;
    if ((type == 7) || (type == 10) || (type == 6)) {
	ps2 = 11;
    }
    if ((type == 8) || (type == 16)) {
	ps2 = 14;
    }
    if (type == 9) {
	ps2 = 85;
    }
    printf("HT%02d C%03d I%02d C%03d\n", type, aa, ignore1, bb);
    if (type == 6) {
	index = 26;
	fseek(file, start + 26, SEEK_SET);
	playscripts = 7;
    } else {
	index = 18;
	fseek(file, start + 18 + playscripts * 4, SEEK_SET);
	uint32_t pointer;
	fread(&pointer, sizeof(pointer), 1, file);
	if (bb != 0) {
	    printf("Error4\n");
	}
    }
    int i = 0;
    for (i = 0; i < playscripts; i++) {
	uint32_t pointer;
	fseek(file, start + index + i * 4, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	if (test_list_32(file, pointer, start, end) > 0) {
	    printf("No Playlist pointer (%08X)\n", pointer);
	} else {
	    parse_playlistpointer(file, pointer, start, end);
	}
    }

    index += 4 * playscripts;

//  if (type != 6)
//    {
    int subgamestart = index;

    if (aa != 0) {
	printf("\n\tSubgames:\t[");
	for (i = 0; i < aa; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    printf("0x%X,", pointer);
	}
	printf("]\n\n");
    }
    index += aa * 4;
    if (type == 6) {
	if (bb != 0) {
	    printf("\n\tSubgames2:\t[");
	    for (i = 0; i < bb; i++) {
		uint32_t pointer;
		fseek(file, start + index + i * 4, SEEK_SET);
		fread(&pointer, sizeof(pointer), 1, file);
		printf("0x%X,", pointer);
	    }
	    printf("]\n\n");
	}
	index += bb * 4;
    }
    index += 20;

    for (i = 0; i < 10; i++) {
	uint32_t pointer;
	fseek(file, start + index + i * 4, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	if (test_list_32(file, pointer, start, end) > 0) {
	    printf("No Playlist pointer (%08X)\n", pointer);
	} else {
	    parse_playlistpointer(file, pointer, start, end);
	}
    }
    index += 10 * 4;

    if (type == 6) {
	uint32_t pointer;
	fseek(file, start + index, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	parse_playlist(file, pointer);
	index += 4;
    } else if (type == 7 || type == 10) {
	uint32_t pointer;
	fseek(file, start + index, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	parse_playlistpointer(file, pointer, start, end);
	index += 4;
    } else if (type == 8) {
	uint32_t pointer;
	fseek(file, start + index, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	parse_oidlist(file, pointer);

	index += 4;
	fseek(file, start + index, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	parse_gameidlist(file, pointer);

	index += 4;
	int i;
	for (i = 0; i < 2; i++) {
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    parse_playlistpointer(file, pointer, start, end);
	}
	index += 2 * 4;
    } else if (type == 9) {
	int i;
	for (i = 0; i < 75; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    parse_playlistpointer(file, pointer, start, end);
	}
	index += 75 * 4;
    } else if (type == 16) {
	uint32_t pointer;
	fseek(file, start + index, SEEK_SET);
	fread(&pointer, sizeof(pointer), 1, file);
	parse_oidlist(file, pointer);

	index += 4;
	int i;
	for (i = 0; i < 3; i++) {
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    parse_playlistpointer(file, pointer, start, end);
	}
	index += 3 * 4;
    }
    if (0) {
	printf("Typ%02d PLP ", type);

	for (i = 0; i < ps2; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_32(file, pointer, start, end);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }

	    if (test_list_32(file, pointer, start, end)) {
//          printf("No Playlist pointer (%08X)\n", pointer);
//          parse_playlistpointer(file, pointer, start, end);
	    } else {
//          parse_playlistpointer(file, pointer, start, end);
	    }
	}
	printf("\n");

	printf("Typ%02d PLY ", type);
	for (i = 0; i < ps2; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_media(file, pointer);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }
	}
	printf("\n");

	printf("Typ%02d OID ", type);
	for (i = 0; i < ps2; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_oid(file, pointer);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }
	}
	printf("\n");

	printf("Typ%02d GID ", type);
	for (i = 0; i < ps2; i++) {
	    uint32_t pointer;
	    fseek(file, start + index + i * 4, SEEK_SET);
	    fread(&pointer, sizeof(pointer), 1, file);
	    int tmp = test_list_gid(file, pointer);
	    if (tmp == 0) {
		printf("+");
	    }
	    if (tmp > 0) {
		printf("-");
	    }
	    if (tmp < 0) {
		printf("0");
	    }
	}
	printf("\n");
    }

    uint32_t firstsub;
    if (type == 6) {
	fseek(file, start + subgamestart + 4, SEEK_SET);
    } else {
	if (aa != 0) {
	    fseek(file, start + subgamestart, SEEK_SET);
	} else {
	    fseek(file, start + 18, SEEK_SET);
	}
    }
    fread(&firstsub, sizeof(firstsub), 1, file);
    if (start + index != firstsub) {
	printf
	    ("start(0x%X) + index(%d) + ps2(%d) * 4 = 0x%X != firstsub(0x%X)\n",
	     start, index, ps2, start + index + ps2 * 4, firstsub);
	printf("Error7!\n");
    }


    for (i = 0; i < aa - 1; i++) {
	uint32_t pointer1, pointer2;
	fseek(file, start + subgamestart + i * 4, SEEK_SET);
	fread(&pointer1, sizeof(pointer1), 1, file);
	fread(&pointer2, sizeof(pointer2), 1, file);
	printf("SubGame(%2d, 0x%X)\n--------------------------\n", i,
	       pointer1);
	parse_subgame(file, pointer1, pointer2, type);
//          print_block(file, pointer1, pointer2, start, end);
    }
    if (type == 6) {
	int sbs2 = subgamestart + 4 * aa;
	for (i = 0; i < bb - 1; i++) {
	    uint32_t pointer1, pointer2;
	    fseek(file, start + sbs2 + i * 4, SEEK_SET);
	    fread(&pointer1, sizeof(pointer1), 1, file);
	    fread(&pointer2, sizeof(pointer2), 1, file);
	    printf("SubGame2(%2d, 0x%X)\n--------------------------\n", i,
		   pointer1);
	    parse_subgame(file, pointer1, pointer2, type);
//          print_block(file, pointer1, pointer2, start, end);
	}
    }
//    }

//    printf("\tType? = %d\n\n", type);

//    fread(&bb, sizeof(bb),1,file);

    if (DUMP_BLOCKS) {

//  if (type == 6)
//    {
	uint32_t test = 0;

	i = 0;
	int b = 0;
	do {
	    i++;
	    fseek(file, start + i, SEEK_SET);
	    b = 0;
	    int j = 0;
	    for (j = 0; j < aa + 5; j++) {
		fread(&test, sizeof(test), 1, file);
		if ((test < start) || (test > end)) {
		    b = 1;
		}
	    }
//      printf("\t%X = %X\n", i, test);

	}
	while ((b == 1) && (i < (end - start - ((aa + 5) * 4))));

	printf("\tType? = %d\t%d\n\n", type, i);

//      fseek(file, start + 18 , SEEK_SET);
//      fread(&test, sizeof(test),1,file);
/*    if ((test < start) || (test > end )) {
//
	printf("%s_game%02d", gme, id);
	printf("\tType? = %d\t%d\n\n", type,i);
    }
*/
/*    if ((aa == 0) || (bb == 0 )) {
//
	printf("%s_game%02d", gme, id);
	printf("\taa = %d\tbb = %d\n\n", aa,bb);
	printf("\tType? = %d\t%d\n\n", type,i);
    }
*/

	uint32_t blocks[5001];
	blocks[0] = start;
	int bi = 1;
	i = 16;
	do {
	    i++;
	    fseek(file, start + i, SEEK_SET);
	    uint32_t testp;
	    fread(&testp, sizeof(testp), 1, file);
	    if ((testp >= start) && (testp < end)) {
		blocks[bi++] = testp;
	    }
	}
	while ((bi < 5000) && (i < (end - start - 3)));
	if (bi < 5000) {
	    blocks[bi++] = end;
	    // sortieren
	    int z = 0;
	    for (i = 0; i < bi; i++) {
		int j;
		for (j = i + 1; j < bi; j++) {
		    if (blocks[i] > blocks[j]) {
			uint32_t tmp = blocks[i];
			blocks[i] = blocks[j];
			blocks[j] = tmp;
		    }
		    if (blocks[i] == blocks[j]
			&& (blocks[i] != 0xffffffff)) {
			blocks[j] = 0xffffffff;
			z++;
		    }
		}
	    }
	    bi -= z;
	    // ausgeben
	    for (i = 0; i < (bi - 1); i++) {
		printf("0x%X B%02d %06d\n\t", blocks[i], i,
		       blocks[i + 1] - blocks[i]);
		uint32_t j;
		int p = 0;
		for (j = blocks[i]; j < blocks[i + 1]; j++) {
		    fseek(file, j, SEEK_SET);
		    uint8_t val;
		    fread(&val, sizeof(val), 1, file);
		    printf("%02X ", val);
		    if (p == 7) {
			printf(" ");
		    }
		    if (p++ == 15) {
			p = 0;
			printf("\n\t");
		    }
		}
		printf("\n\t\t");
		int coun = 0;
		uint32_t last = 0;
		int k = 0;
		for (j = blocks[i]; j < blocks[i + 1]; j++) {
		    fseek(file, j, SEEK_SET);
		    uint32_t point;
		    fread(&point, sizeof(point), 1, file);
		    if ((point >= start) && (point < end)) {
			j += 3;
			if (last < point) {
			    coun++;
			    last = point;
			} else {
			    printf("B%02d TP%02d I%02d L%02d \n\t\t", i,
				   type, k++, coun);
			    if ((i == 0) && (k == 2) && (coun != aa)
				&& (aa != 0))
				printf("Error!\n");
			    coun = 1;
			    last = 0;
			}
		    } else {
			if (coun > 0) {
			    if ((coun > 1) || (k > 0)) {
				printf("B%02d TP%02d I%02d L%02d \n\t\t",
				       i, type, k++, coun);
				if ((i == 0) && (k == 2) && (coun != aa)
				    && (aa != 0))
				    printf("Error!\n");
			    }
			    coun = 0;
			    last = 0;
			}
		    }
		}
		if (coun > 0) {
		    printf("B%02d TP%02d I%02d L%02d \n\t\t", i, type, k++,
			   coun);
		    if ((i == 0) && (k == 2) && (coun != aa) && (aa != 0))
			printf("Error!\n");
		    coun = 0;
		    last = 0;
		}
		printf("\n");
	    }
	}
    }
}

void parse_games(FILE * file, uint32_t start)
{

    uint32_t count = 0;
    fseek(file, start, SEEK_SET);
    fread(&count, sizeof(count), 1, file);

    printf("\tCount = %d\n\n", count);

    count_games = count;

    int i;
    for (i = 0; i < (count); i++) {
	uint32_t start_script;
	uint32_t next_script;
	fseek(file, (start + 4) + 4 * i, SEEK_SET);
	fread(&start_script, sizeof(start_script), 1, file);
	if (i < (count - 1)) {
	    fread(&next_script, sizeof(next_script), 1, file);
	} else {
	    next_script = end_game;
	}
	char name[2024];
	snprintf(name, 2020, "game%02d", i);
	save_block(name, start_script, next_script - start_script);

	printf("Game Header (%2d, 0x%X)\n--------------------------\n", i,
	       start_script);
	parse_game(file, i, start_script, next_script);
    }
}

int main(int argc, char *argv[])
{

    gme = argv[1];

    FILE *file = fopen(gme, "rb");

    prepare_tests(file);

    uint32_t start_game = 0;
    fseek(file, 0x10, SEEK_SET);
    fread(&start_game, sizeof(start_game), 1, file);

//  parse_list_16(file, p_unknown_table);
//  printf ("Scripts:\n");
//  print_block (file, 0x200, start_game, 0, file_size);
//  parse_list_16(file, p_unknown_table);
//    printf("Unknown (0x%X - 0x%X) = (%d)\n", unknown_table, media_table , media_table - unknown_table);
//  print_block (file, unknown_table, media_table, 0, file_size);

    printf("Game (%s)\n", gme);
    printf("Games (0x%X)\n--------------------------\n", start_game);
    parse_games(file, start_game);

    fclose(file);
}
