      subroutine read_asc(model,*)       !  read common from modelW.ASC
      use voacapl_defs
      use crun_directory
      character model*6

      INCLUDE 'ficepac.hdr'
      INCLUDE 'fice_ssn.hdr'
      common /cbotlines/ nbotlines,linesbot(14)
      common /ctoplines/ ntoplines,linestop( 7)
      common /zMETHOD/ kmeth     !  propagation method used for method=23
         integer*4 kmeth
c jw      common /crun_directory/ run_directory
c jw         character run_directory*50
c**********************************************************************

      nch_run=lcount(run_directory,50)
      open(29,file=run_directory(1:nch_run)//PATH_SEPARATOR//model//'w.asc',
     +      status='old',err=999)
      rewind(29)
      i999=0

      read (29,1,err=900) method,methodname
1     format(i2,1x,a)
      read (29,2,err=900) icoeffs,year
2     format(11i6)
      read (29,3,err=900) montha
3     format(10f7.2)
      read (29,4,err=900) ssna
4     format(10i7)
      read (29,3,err=900) qindexa
      read (29,2,err=900) ihr1,ihr2,ihrinc,itimecode
      read (29,5,err=900) tlatdeg,tlondeg,tname
5     format(3a)
      read (29,5,err=900) rlatdeg,rlondeg,rname
      read (29,6,err=900) ipath,gcdkm,gcdnmi,gcdmi,TazR
6     format(i5,3f10.2,f10.4)
      read (29,2,err=900) freq
      read (29,7,err=900) noise,amind,xlufp,rsn,pmp,dmpx,fprob
7     format(i5,f10.6,2i5,2f10.4,4f10.5)
      read (29,8,err=900) rec_dir,rec_file,rec_bear,rec_gain
8     format(a,1x ,a,2f10.5)
      read (29,2,err=900) numants,Index_ant
      do 10 i=1,numants
       read (29,9,err=900) 
     +            minfreq(i),maxfreq(i),design_freq(i),xmtr_dir(i),
     +            xmtr_file(i),xmtr_model(i),beam_main(i),TxPower(i)
9     format(2i5,f10.5,1x,a,1x ,a,1x ,a,f10.5,f20.10)
10    continue
      read (29,11,err=900) nbotlines,linesbot
11    format(20i4)
      read (29,11,err=900) ntoplines,linestop
      read (29,12,err=900) flat
12    format(5f15.8)
      read (29,12,err=900) flon
      read (29,11,err=900) utime
      read (29,12,err=900) fof2
      read (29,12,err=900) ssn_eff
      read (29,12,err=900) ssn_ave
      read (29,1,err=900) kmeth
      read (29,2,err=900) i999

      close(29)
      if(i999.ne.999) go to 999
      return
900   close(29)
999   return 1      !  error return
      END
* -------------------------------------------------------------------- *
