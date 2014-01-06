function printjumptablelines(fid, jumptable, jumptable_size)
    for i = 1:jumptable_size-1 % we don't know yet how far the last entry goes
        len = jumptable(i+1) - jumptable(i);
        fprintf('  Length of referenced jump table line %d at %d / 0x%04X calculated as %d / 0x%02X\n', i, jumptable(i), jumptable(i), len, len);
        fseek(fid, jumptable(i), 'bof');
        data = fread(fid, len, 'uint8');
        decodejumptableline(data);
    end
    i = jumptable_size;
    fprintf('  Length of referenced jump table line %d at %d / 0x%02X is unknown\n', i, jumptable(i), jumptable(i));
end
