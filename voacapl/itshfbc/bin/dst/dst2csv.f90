! A small application to convert a .DST binary record file into a text based csv.
!
! J.Watson, February 2018
!
program dst2csv
    logical*1 file_exists
    REAL :: gcdkm, xlat, xlon, MUF, FOT, ANGLE, DELAY, VHITE, MUFday, LOSS
    REAL :: DBU, SDBW, NDBW, SNR, RPWRG, REL, MPROB, SPRB, SIGLW, SIGUP, SNRLW, SNRUP
    REAL :: TGAIN, RGAIN, SNRxx, DBM
    INTEGER :: num_args
    CHARACTER(len=128) :: itshfbc_path = ""
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
    CHARACTER(120) :: RUN_DIR
    CHARACTER(len=1), parameter :: PATH_SEPARATOR ='/'
    CHARACTER(len=128) :: idx_path, dst_path

    num_args = command_argument_count()
    
    if (num_args == 1) then
        CALL get_command_argument(1, itshfbc_path)
        idx_path = trim(itshfbc_path)//PATH_SEPARATOR//'run'//PATH_SEPARATOR//'voacapd.idx'
        dst_path = trim(itshfbc_path)//PATH_SEPARATOR//'run'//PATH_SEPARATOR//'voacapd.dst'
    else
        idx_path = 'voacapd.idx'
        dst_path = 'voacapd.dst'
    end if
    
    inquire(file=idx_path, exist=file_exists)
    if (.NOT.file_exists) then
        stop 'Unable to open IDX file '//idx_path
    end if

    inquire(file=dst_path, exist=file_exists)
    if (.NOT.file_exists) then
        stop 'Unable to open DST file '//idx_path
    end if

    !print *, "Opening IDX file"
    Open(IDX_FILE, file=idx_path, status='old')
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
    open(DST_FILE,file=dst_path,status='old', form='unformatted',access='direct',recl=108)
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
