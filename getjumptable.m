function [jumptable jumptable_size] = getjumptable(fid, jumptable_p)
    fseek(fid, jumptable_p, 'bof');
    jumptable_size = fread(fid, 1, 'uint16');
    fprintf('  Jump table size in first 16 bit: %d / 0x%08X found at %d / 0x%08X\n', ...
        jumptable_size, jumptable_size, jumptable_p, jumptable_p);

    jumptable = [];
    for i = 1:jumptable_size
        offset = fread(fid, 1, 'uint32');
        jumptable = [jumptable; offset];
    end
end
