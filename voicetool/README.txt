The tiptoi firmware files contain a block of wav files that are used by
the pen on various occations, e.g. startup, shutdown and debug mode
(see wavs.txt for a complete list).

As the original firmware contains spoken chinese it is difficult to
use the debug mode.

This is where the tools from this subdir kick in:
- voicetool.py can be used to extract the wav files from a
  Chomp_Voice.bin file or compile a new Chomp_Voice.bin file from
  existing wav files.
- gen_tts_files.sh generates all needed wav files (english/german mix
  currently) using text-to-speech


Usage of voicetool.py:
----------------------

python voicetool.py compile [--numwavs <n>] <basename> <outputfile>
python voicetool.py decompile <inputfile> <basename>


python voicetool.py compile firmware_tts new.bin

This will build a file called new.bin in the current directory that
consists of the firmware_tts_00.wav to firmware_tts_47.wav. Default is
to use 48 input wavs. This can be modified using the numwavs
parameter, but this is not useful for anything besides testing. The
resulting file new.bin can then be integrated into a new firmware
file.


To extract the wavs from a voice file use voicetool like this:

python voicetool.py decompile Chomp_Voice.bin orig

This will write orig_00.wav to orig_47.wav to the current directory.

Handling firmware files
-----------------------

To get the Chomp_Voice.bin from a firmware file (Update3202.upd) you
can load the upd file into burntool using the "import" function. This
will write all contained files to disk into a directory called
Update_Files:

Update_Files
Update_Files/PROG.bin
Update_Files/voice
Update_Files/voice/Chomp_Voice.bin  <-------
Update_Files/BOOT.bin
Update_Files/codepage.bin
Update_Files/producer.bin
Update_Files/Language
Update_Files/Language/BatLowUpdateFRENCH.wav
Update_Files/Language/BatLowUpdateDUTCH.wav
Update_Files/Language/UpdateGERMAN.wav
Update_Files/Language/UpdateDUTCH.wav
Update_Files/Language/BatLowUpdateGERMAN.wav
Update_Files/Language/UpdateFRENCH.wav

After having created a new Chomp_Voice.bin file using voicetool.py you
can simply copy that file to the same place while burntool is still
running and the use the "export" function. (It is recommended to use
burntool5 for this.)

Maybe some day we will have an own tool to extract the files from the
firmware.

Next you need to force the pen to use that new firmware update
file. As you probably have the current firmware on your pen already it
will ignore the newly created file as it has the same version
number. Patching the version number in the upd file does not work (for
unknown reasons). So the trick is to change the language of the pen to
something else using the procedure described in the wiki
(https://github.com/entropia/tip-toi-reveng/wiki/Firmware). After the
language has been switched successfully repeat the procedure to change
back to your original language using the newly created firmware file.

Alternatively you can use burntool to flash the new upd file to your
pen. Check the tip-toi-reveng wiki for information how to do that.
