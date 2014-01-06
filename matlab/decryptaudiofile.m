function decryptedaudiofile = decryptaudiofile(encryptedaudiofile, magic)
    if(nargin == 1)
        fprintf('    Trying to calculate magic from file as magic is not given.\n');
        magic = bitxor(encryptedaudiofile(1), 79); % 79 = ASCII 'O' from 'OggS'
        % TODO: add support for RIFF WAVE
    end
    
    fprintf('    Decrypting audio file with magic: %d / 0x%02X\n', magic, magic);
    magix = bitxor(magic, 255);
    decryptedaudiofile = encryptedaudiofile;
    idx = find( (encryptedaudiofile ~= 255) & (encryptedaudiofile ~= 0) ...
        & (encryptedaudiofile ~= magic) & (encryptedaudiofile ~= magix) );
    decryptedaudiofile(idx) = bitxor(encryptedaudiofile(idx), magic);
end
