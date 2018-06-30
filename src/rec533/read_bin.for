      subroutine read_bin(model,*)       !  read common from modelW.BIN
      use voacapl_defs
      use crun_directory
      character model*6

      INCLUDE 'FICEPAC.hdr'
      INCLUDE 'FICE_SSN.hdr'
      common /cbotlines/ nbotlines,linesbot(14)
      common /ctoplines/ ntoplines,linestop( 7)
c      common /crun_directory/ run_directory
c         character run_directory*50
c**********************************************************************
      nch_run=lcount(run_directory,50)
      open(29,file=trim(root_directory)//PATH_SEPARATOR//model//'w.bin', form='unformatted',status='old',err=999)
      rewind(29)
      i999=0
      read(29,err=900) method,methodname,icoeffs,year
     @       ,montha,ssna,qindexa,ihr1,ihr2
     @       ,ihrinc,itimecode
     @       ,tlatdeg,tlondeg,tname
     @       ,rlatdeg,rlondeg,rname
     @       ,ipath,gcdkm,gcdnmi,gcdmi
     @       ,TazR,freq,noise
     @       ,amind,xlufp,rsn,pmp,dmpx,fprob
     @       ,rec_dir,rec_file,rec_bear,rec_gain
     +       ,numants,Index_ant,minfreq,maxfreq,design_freq
     +       ,xmtr_dir,xmtr_file,xmtr_model,beam_main,TxPower
     +       ,nbotlines,linesbot,ntoplines,linestop
     +       ,flat,flon,utime,fof2,ssn_eff,ssn_ave
     +       ,i999
      close(29)
      if(i999.ne.999) go to 999
      return
900   close(29)
999   return 1      !  error return
      END
* -------------------------------------------------------------------- *
