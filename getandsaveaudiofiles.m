function audiofilenames = getandsaveaudiofiles(fid, audiotable, prefix)
    audiotable_size = size(audiotable, 1);
    audiofilenames = [];
    for i = 1:audiotable_size
        offset = audiotable(i, 1);
        length = audiotable(i, 2);
        encryptedaudiofile = getaudiofile(fid, offset, length);
        decryptedaudiofile = decryptaudiofile(encryptedaudiofile);
        audiofilename = saveaudiofile(offset, decryptedaudiofile, prefix, i);
        %audioinfo(audiofilename);
        audiofilenames = [audiofilenames; audiofilename];
    end
end
