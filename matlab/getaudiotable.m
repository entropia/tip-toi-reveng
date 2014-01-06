function [audiotable] = getaudiotable(fid)

    fseek(fid, 4, 'bof');
    audiotable_p = fread(fid, 1, 'uint32');
    fprintf('Audio table found at %d / 0x%08X\n', audiotable_p, audiotable_p);

    fseek(fid, audiotable_p, 'bof');
    first = fread(fid, 1, 'uint32');
    fseek(fid, audiotable_p, 'bof');

    offset = 0;
    audiotable = [];
    while( ftell(fid) < first)
        offset = fread(fid, 1, 'uint32');
        length = fread(fid, 1, 'uint32');
        % fprintf('Offset: %d / 0x%08X, Length: %d / 0x%04X\n', offset, offset, length, length);
        audiotable = [audiotable; offset length];
    end
    fprintf('  %d entries found in audiotable\n', size(audiotable, 1));

end
