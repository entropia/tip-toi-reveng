function decode(filename)
    clc

    % open file for reading
    fid = fopen(filename, 'r');

    % read whole file content
    % content = fread(fid);

    % get audio table
    audiotable = getaudiotable(fid);

    % get and save all audio files
    %getandsaveaudiofiles(fid, audiotable, filename);

    %get maintable
    maintable = getmaintable(fid);

    %print jump tables which are referenced by the main table
    %printjumptables(fid, maintable);

    % close file
    fclose(fid);
end
