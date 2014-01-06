function decodejumptableline(data)
    % TODO: implement more complex function to decode bytestream and not
    % only beginning of line
    len = length(data);
    %fprintf('(%d) ', len);
    fprintf('    '); fprintf('%02X ', data); fprintf('\n');
    if( (data(6) == 249) & (data(7) == 255) & (data(8) == 01) ) % 0xF9 0xFF 0x01
        % endianness! 16 bit value
        nnnn = bitxor( (bitshift(data(10), 8)), data(9) );
        if( data(11) == 0 )
            xx = data(12);
            fprintf('    F1(%d,%d)\n', nnnn, xx);
        else
            yy = data(11);
            xx = bitxor( (bitshift(data(13), 8)), data(12) );
            fprintf('    F2(%d,%d,%d)\n', nnnn, yy, xx);
        end
    else
        fprintf('    currently unknown\n');
    end
end
