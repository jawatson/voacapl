program dst2ascii
    REAL :: gcdkm, xlat, xlon, MUF, FOT, ANGLE, DELAY, VHITE, MUFday, LOSS
    REAL :: DBU, SDBW, NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, SNRLW, SNRUP
    REAL :: TGAIN, RGAIN, SNRxx, DBM
    INTEGER, PARAMETER :: NUMDIST = 51
    INTEGER, PARAMETER :: NUMFREQ = 8
    INTEGER, PARAMETER :: HOURBLK = NUMDIST * NUMFREQ
    INTEGER :: ptr, utcPtr, freqPtr
    CHARACTER(4) :: xmode
    print *, "Opening DST file"
    open(20,file='voacapd.dst',status='old', form='unformatted',access='direct',recl=108)
    open(30,file='voacapd.asc')
    rewind(30)
    DO utcPtr = 1, 24
        write(30, '(AI2)') "UTC=", utcPtr
        DO freqPtr = 1, 8
            write(30, '(AI2)') "Freq:", freqPtr
            write(30, '(AI2)') " id   gcdkm  Latitude Longitude Mode  MUF     FOT     ANGLE   &
                    DELAY   VHITE   MUFday  LOSS    DBU     SDBW    NDBW    SNR     RPWRG   &
                    REL     MPROB   SPRB    SIGLW   SIGUP   SNRLW   SNRUP   TGAIN   RGAIN   &
                    SNRxx   DBM"
            DO ptr = NUMDIST-1, 0, -1
                read(20, rec=((utcPtr-1)*HOURBLK)+(ptr*8)+freqPtr ) gcdkm,xlat,xlon,xmode, MUF, &
                    FOT, ANGLE, DELAY, VHITE, MUFday, LOSS, DBU, SDBW, &
                    NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, &
                    SNRLW, SNRUP, TGAIN, RGAIN, SNRxx, DBM
                write(30, '(I3, F8.1, 2F10.4, A5, 23F8.3)') NUMDIST-ptr, gcdkm, xlat, xlon, xmode, MUF, FOT, ANGLE, &
                    DELAY, VHITE, MUFday, LOSS, DBU, SDBW, NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, &
                    SNRLW, SNRUP, TGAIN, RGAIN, SNRxx, DBM
            END DO
        END DO
    END DO
end program dst2ascii
