program dst2csv
    REAL :: gcdkm, xlat, xlon, MUF, FOT, ANGLE, DELAY, VHITE, MUFday, LOSS
    REAL :: DBU, SDBW, NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, SNRLW, SNRUP
    REAL :: TGAIN, RGAIN, SNRxx, DBM
    INTEGER :: ios
    INTEGER :: NUMDIST
    INTEGER :: NUMFREQ
    INTEGER :: NUMHOUR
    REAL, DIMENSION(1 : 20) :: FREQS
    INTEGER, PARAMETER :: IDX_FILE = 70
    INTEGER, PARAMETER :: DST_FILE = 80
    INTEGER, PARAMETER :: CSV_FILE = 90
    INTEGER :: HOURBLK
    INTEGER :: ptr, utcPtr, freqPtr, offset, id = 0
    CHARACTER(200) :: index_buffer
    CHARACTER(4) :: xmode

    !print *, "Opening IDX file"
    Open(IDX_FILE, file='voacapd.idx', status='old')
    read(IDX_FILE, '(I5A)') NUMDIST, index_buffer
    !print *, NUMDIST
    read(IDX_FILE, '(I3A)') NUMFREQ, index_buffer
    !print *, NUMFREQ

    DO freqPtr = 1, NUMFREQ
        offset = 1 + (7*(freqPtr-1))
        read(index_buffer((offset): (offset+7)), '(F7.3)') FREQS(freqPtr)
        !print *, FREQS(freqPtr)
    END DO
    
    HOURBLK = NUMDIST * NUMFREQ
    close(IDX_FILE)
    !print *, "Opening DST file"
    open(DST_FILE,file='voacapd.dst',status='old', form='unformatted',access='direct',recl=108)
    open(CSV_FILE,file='voacapd.csv')
    rewind(CSV_FILE)
    write(CSV_FILE, '(A)') "id,utc,chan,freq,gcdkm,Latitude,Longitude,Mode,MUF,FOT,ANGLE,DELAY,VHITE,MUFday,LOSS, &
        DBU,SDBW,NDBW,SNR,RPWRG,REL,MPROB,SPRB,SIGLW,SIGUP,SNRLW,SNRUP,TGAIN,RGAIN,SNRxx,DBM"

    DO utcPtr = 1, 24
        DO freqPtr = 1, NUMFREQ
            DO ptr = NUMDIST-1, 0, -1
                id = id + 1
                read(DST_FILE, rec=((utcPtr-1)*HOURBLK)+(ptr*8)+freqPtr ) gcdkm,xlat,xlon,xmode, MUF, &
                    FOT, ANGLE, DELAY, VHITE, MUFday, LOSS, DBU, SDBW, &
                    NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, &
                    SNRLW, SNRUP, TGAIN, RGAIN, SNRxx, DBM
                write(CSV_FILE, '(3(I0,","), F0.3, A, F0.1, A, 2(F0.4,","), A4, 23(",",F0.3))') &
                    id, utcPtr, freqPtr, FREQS(freqPtr), ",", gcdkm, ",", xlat, xlon, xmode, MUF, FOT, ANGLE, &
                    DELAY, VHITE, MUFday, LOSS, DBU, SDBW, NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, &
                    SNRLW, SNRUP, TGAIN, RGAIN, SNRxx, DBM
            END DO
        END DO
    END DO
    close(DST_FILE)
    close(CSV_FILE)
end program dst2csv
