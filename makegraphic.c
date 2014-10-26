#include <png.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <getopt.h>

/* A coloured pixel. */

typedef struct {
    uint8_t set;
} pixel_t;

/* A picture. */

typedef struct  {
    pixel_t *pixels;
    size_t width;
    size_t height;
} bitmap_t;

int space = 10;
int width = 2;
static int verbose_flag = 0;
static int ting_flag = 0;

/* Given "bitmap", this returns the pixel of bitmap at the point
   ("x", "y"). */

static pixel_t * pixel_at (bitmap_t * bitmap, int x, int y)
{
    return bitmap->pixels + bitmap->width * y + x;
}

/* Write "bitmap" to a PNG file specified by "path"; returns 0 on
   success, non-zero on error. */

static int save_png_to_file (bitmap_t *bitmap, const char *path)
{
    FILE * fp;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    size_t x, y;
    png_byte ** row_pointers = NULL;
    /* "status" contains the return value of this function. At first
       it is set to a value which means 'failure'. When the routine
       has finished its work, it is set to a value which means
       'success'. */
    int status = -1;
    /* The following number is set by trial and error only. I cannot
       see where it it is documented in the libpng manual.
    */
    int pixel_size = 3;
    int depth = 8;

    fp = fopen (path, "wb");
    if (! fp) {
        goto fopen_failed;
    }

    png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL) {
        goto png_create_write_struct_failed;
    }

    info_ptr = png_create_info_struct (png_ptr);
    if (info_ptr == NULL) {
        goto png_create_info_struct_failed;
    }

    /* Set up error handling. */

    if (setjmp (png_jmpbuf (png_ptr))) {
        goto png_failure;
    }

    /* Set image attributes. */

    png_set_IHDR (png_ptr,
                  info_ptr,
                  bitmap->width,
                  bitmap->height,
                  depth,
                  PNG_COLOR_TYPE_RGB,
                  PNG_INTERLACE_NONE,
                  PNG_COMPRESSION_TYPE_DEFAULT,
                  PNG_FILTER_TYPE_DEFAULT);
	/* Set the resolution */
	png_set_pHYs(png_ptr, info_ptr, 47244, 47244,
	        PNG_RESOLUTION_METER)	;
    /* Initialize rows of PNG. */

    row_pointers = png_malloc (png_ptr, bitmap->height * sizeof (png_byte *));
    for (y = 0; y < bitmap->height; ++y) {
        png_byte *row =
            png_malloc (png_ptr, sizeof (uint8_t) * bitmap->width * pixel_size);
        row_pointers[y] = row;
        for (x = 0; x < bitmap->width; ++x) {
            pixel_t * pixel = pixel_at (bitmap, x, y);
            *row++ = (pixel->set)?0:255;
            *row++ = (pixel->set)?0:255;
            *row++ = (pixel->set)?0:255;
        }
    }

    /* Write the image data to "fp". */

    png_init_io (png_ptr, fp);
    png_set_rows (png_ptr, info_ptr, row_pointers);
    png_write_png (png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

    /* The routine has successfully written the file, so we set
       "status" to a value which indicates success. */

    status = 0;

    for (y = 0; y < bitmap->height; y++) {
        png_free (png_ptr, row_pointers[y]);
    }
    png_free (png_ptr, row_pointers);

 png_failure:
 png_create_info_struct_failed:
    png_destroy_write_struct (&png_ptr, &info_ptr);
 png_create_write_struct_failed:
    fclose (fp);
 fopen_failed:
    return status;
}

typedef struct {
	uint8_t value[9];
} field;

static int calculateChecksum(int dec) {
	int checksum = 0;

	if(ting_flag){
		checksum  = (((dec >> 2) ^ (dec >> 8) ^ (dec >> 12) ^ (dec >> 14)) & 0x01) << 1;
		checksum |= (((dec) ^ (dec >> 4) ^ (dec >>  6) ^ (dec >> 10)) & 0x01);
	}else{
		checksum  = (((dec >> 2) ^ (dec >> 8) ^ (dec >> 12) ^ (dec >> 14)) & 0x01) << 1;
		checksum |= (((dec) ^ (dec >> 4) ^ (dec >>  6) ^ (dec >> 10)) & 0x01);
		checksum ^= 0x02; //needs to be tested
	}
	return checksum;
}

field create_field(int value){
	field tmp = {.value = 0};
	int counter = 0;
	tmp.value[8] = calculateChecksum(value);
	while (value){
		tmp.value[counter] = value%4;
		value = value/4;
		counter++;
	}
	return tmp;
}
/* Sets a single pixel to the absolute x and y coordinate */
void set_pixel(bitmap_t* image, int x, int y){
	pixel_t* pixel = pixel_at (image, x, y);
	if(verbose_flag){
		printf("Set pixel at %i, %i\n", x,y);
	}
	pixel->set = 1;
}
/* Sets a set of 4 pixel, x and y are absolute */
void set_pixels(bitmap_t* image, int x, int y){
	set_pixel(image, x, y);
	set_pixel(image, x+1, y);
	set_pixel(image, x, y+1);
	set_pixel(image, x+1, y+1);
}

void makeAbsoluteCoordinates(int* x, int* y){
	*x*=(width + space);
	*y*=(width + space);
}

void set_frame(bitmap_t* image, int x, int y){
	makeAbsoluteCoordinates(&x, &y);
	set_pixels(image, x, y);
}
void set_frame_marker(bitmap_t* image, int x, int y){
	makeAbsoluteCoordinates(&x, &y);
	set_pixels(image, x+1, y);
}

void set_point(bitmap_t* image, int value, int x, int y){
	makeAbsoluteCoordinates(&x, &y);
	x += ((value == 0 || value == 3)?1:-1)*2;
	y += ((value == 0 || value == 1)?1:-1)*2;
	set_pixels(image, x, y);
}

int main (int argc, char* argv[])
{
    bitmap_t image;
    int x;
    int y;
	field myfield;
	char *filename;
	int c;

    image.width = 1000;
    image.height = 1000;

    int option_index = 0;
	while (1)
    {
      static struct option long_options[] = {
		  {"sizex",required_argument,0,'x'},
		  {"sizey",required_argument,0,'y'},
		  {"file",required_argument,0,'f'},
		  {0,0,0,0}};
	  /* getopt_long stores the option index here. */

      c = getopt_long (argc, argv, "x:y:f:",
                       long_options, &option_index);

      /* Detect the end of the options. */
      if (c == -1)
        break;

      switch (c)
        {
        case 'x':
          image.width = atoi(optarg);
          break;

        case 'y':
          image.height = atoi(optarg);
          break;

        case 'f':
          filename = optarg;
          break;

        case '?':
          /* getopt_long already printed an error message. */
          break;

        default:
          abort ();
        }
    }

	myfield = create_field(atoi(argv[option_index]));
	printf("Checksum is: %i\n", myfield.value[8]);

    /* Create an image. */

    image.pixels = calloc (sizeof (pixel_t), image.width * image.height);

    for (y = 0; y < image.height/(space + width); y++) {
        for (x = 0; x < image.width/(space + width); x++) {
			int fieldx = x & 3;
			int fieldy = y & 3;
			if(fieldx == 0 && fieldy == 2){
				set_frame_marker(&image, x,y);
			}else
			if(fieldx == 0 || fieldy == 0){
				set_frame(&image, x,y);
			}
			else{
				set_point(&image, myfield.value[8-(fieldx-1)-((fieldy-1)*3)], x,y);
			}
        }
    }

    /* Write the image to a file 'fruit.png'. */

    save_png_to_file (&image, filename);

    return 0;
}
