function maintable = getmaintable(fid)

    fseek(fid, 0, 'bof');
    maintable_p = fread(fid, 1, 'uint32');
    fprintf('Main table found at %d / 0x%08X\n', maintable_p, maintable_p);
    
    fseek(fid, maintable_p, 'bof');
    unknown1 = fread(fid, 1, 'uint32');
    unknown2 = fread(fid, 1, 'uint32');
    fprintf('  1st unknown entry (32 bit) %d / 0x%08X\n', unknown1, unknown1);
    fprintf('  2nd unknown entry (32 bit) %d / 0x%08X\n', unknown2, unknown2);
    
    data = 0;
    maintable = [];
    while data ~= 4294967295 % 0xFFFFFFFF
        pos = ftell(fid);
        data = fread(fid, 1, 'uint32');
        fprintf('  %d / 0x%08X (at pos %d / 0x%08X)\n', data, data, pos, pos);
        maintable = [maintable; data];
    end
    maintable = maintable(1:end-1);
    fprintf('  Stopped because pattern 0xFFFFFFFF was found\n');
    
end
