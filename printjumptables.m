function printjumptables(fid, maintable)

    %print data entries of maintable = jumptables
    for i = 1:length(maintable)
        jumptable_p = maintable(i);
        fprintf('Jump table at %08X:\n', jumptable_p);
        [jumptable jumptable_size] = getjumptable(fid, jumptable_p);

        %print jumptable lines
        printjumptablelines(fid, jumptable, jumptable_size);
    end

end
