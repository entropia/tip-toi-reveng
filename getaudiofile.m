function audiofile = getaudiofile(fid, offset, length)
    fprintf('  Getting audio file from Offset: %d / 0x%08X, Length: %d / 0x%04X\n', ...
        offset, offset, length, length);
    fseek(fid, offset, 'bof');
    audiofile = fread(fid, length, 'uint8');
end
