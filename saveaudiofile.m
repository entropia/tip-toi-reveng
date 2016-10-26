function filename = saveaudiofile(offset, decryptedaudiofile, prefix, idx)
    %filename = lower( dec2hex(offset, 8) );
    % offset is no longer used but index
    filename = sprintf('%03d', idx-1);
    prefix( prefix == ' ' ) = '_';
    filename = ['./oggs/' prefix '_' filename '.ogg'];
    afid = fopen(filename, 'w+');
    fprintf('    Saving audio to file %s\n', filename);
    fwrite(afid, decryptedaudiofile, 'uint8');
    fclose(afid);
end
