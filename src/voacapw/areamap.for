      subroutine areamap(area,filename,fileout,meth)
c**********************************************************
c          Execute with:
c                call areamap(area,filename,fileout,meth)
c          where:
c                area     = 'A' = normal  area coverage
c                         = 'I' = INVERSE area coverage
c                filename = input data file name to process
c                fileout  = output data file name to create
c                           If method={c,p} make fileout.da? files
c                meth     = 'm' = map only
c                         = 'c' = map with contours
c                         = 'p' = calculations & plots
c**********************************************************
      use voacapl_defs
      use crun_directory
      INCLUDE 'ficearea.hdr'
c jw      common /crun_directory/ run_directory
c jw         character run_directory*50
      character fileout*64,filename*64,ich,meth*1,model*6
      character grid_file*70,area*1
      dimension layers(6),areafreqs(11)
      integer bandw
c jw      integer*2 error_code
      integer error_code
c jw      character tns*1,tew*1,pns*1,pew*1,alf*80,sufix*4,path*5,coeffs*4
      character tns*1,tew*1,pns*1,pew*1,alf*80,path*5,coeffs*4
      character(len=5) :: sufix
      character recfile*21,xmtrfile*21,cirafz*30
c      character system_type*4,beam_alf*5,card*90,data_dir*9
      character system_type*4,beam_alf*5
      character(len=VOA_PATH_LEN) :: data_dir 
      character(len=max(12+MAX_AREA_MONTHS*7, 90)) :: card
      character(len=20) :: fmt_str

c jw      data system_type/'DOS '/
      data system_type/'UNIX'/
      data iyear/1993/
c jw      call DOScolr                      !  read color table
c jw      call WINcolr                      !  read Windows color table

      data_dir=trim(area_directory)//PATH_SEPARATOR
      if(area.eq.'I') data_dir=trim(area_inv_directory)//PATH_SEPARATOR

      open(29,file=trim(data_dir)//filename,status='old',iostat=ios,err=900)
      rewind(29)
      cirafz='      '
800   read(29,'(a)',end=890) card
      if(card(1:10).eq.'Model    :') then
         model=card(11:16)
      else if(card(1:10).eq.'Colors   :') then
         alf=card(11:90)
c          read COLOR & CITYNAME whether OLD or NEW format
         call col_FMT(29,alf,color,cityname)
      else if(card(1:10).eq.'Nparms   :') then
         read (card(11:15),'(i5)') nparms
         do 803 i=1,5
         parms(i)='      '
         nlevels(i)=0
         do 803 j=1,10
         conlevels(j,i)=-1
803      ishades(j,i)=-1
         do 805 i=1,nparms
805      read (29,806) parms(i),nlevels(i),
     +             (ishades(j,i),conlevels(j,i),j=1,nlevels(i))
806      format(10x,a,i4,10(i3,1x,f7.2))
      else if(card(1:10).eq.'Transmit :' .or. 
     +        card(1:10).eq.'Receive  :') then
         tlatdeg=card(11:20)
         tlondeg=card(21:30)
         tname=card(31:50)
         path=card(52:56)
         if(path(1:1).eq.'L') then
            path='Long '
            npsl=1
         else
            path='Short'
            npsl=0
         end if
      else if(card(1:10).eq.'Pcenter  :') then
         platdeg=card(11:20)
         plondeg=card(21:30)
         pname=card(31:50)
      else if(card(1:10).eq.'Area     :') then
         read(card(11:50),'(4f10.1)') xmin,xmax,ymin,ymax
      else if(card(1:10).eq.'Gridsize :') then
         read(card(11:20),'(2i5)') ngrid,gridtype
      else if(card(1:10).eq.'Method   :') then
         read(card(11:15),'(i5)') method
      else if(card(1:10).eq.'Coeffs   :') then
         coeffs=card(11:14)
      ! The following inputs have been modified to work with a larger
      ! number of area plots
      else if(card(1:10).eq.'Months   :') then
         write (fmt_str, "(AI0A)") "(10x,", MAX_AREA_MONTHS,"f7.2)"
         read(card, fmt_str) montha
         do 305 i=1,MAX_AREA_MONTHS     !  fix old integer months (before MONTH.DAY)
           if(montha(i).gt.0. .and. montha(i).lt.1.) montha(i)=montha(i)*100.
305      continue
      else if(card(1:10).eq.'Ssns     :') then
         write (fmt_str, "(AI0A)") "(10x,", MAX_AREA_MONTHS,"I7)"
         read(card, fmt_str) ssna
      else if(card(1:10).eq.'Qindexs  :') then
         write (fmt_str, "(AI0A)") "(10x,", MAX_AREA_MONTHS,"I7)"
         read(card,fmt_str) qindexa
      else if(card(1:10).eq.'Hours    :') then
         write (fmt_str, "(AI0A)") "(10x,", MAX_AREA_MONTHS,"I7)"
         read(card, fmt_str) ihour
      else if(card(1:10).eq.'Freqs    :') then
         write (fmt_str, "(AI0A)") "(10x,", MAX_AREA_MONTHS,"f7.3)"
         read(card, fmt_str) Freq
      else if(card(1:10).eq.'System   :') then
         if(model.eq.'ICEPAC' .or. model.eq.'VOACAP') then
            read (card,810) noise,amind,xlufp,rsn,pmp,dmpx
810         format(10x,i5,f10.3,2i5,2f10.3)
         else if(model.eq.'REC533') then
            read (card,811) noise,amind,xlufp,rsn,bandw
811         format(10x,i5,f10.3,2i5,i10)
         end if
      else if(card(1:10).eq.'Fprob    :') then
            read(card,'(10x,4f5.2)') fprob
      else if(card(1:10).eq.'Rec Ants :') then
         read(card,812)  recfile,rec_gain,rec_bear
812      format(10x,1x,a21,1x,7x,2f6.1)
      else if(card(1:10).eq.'Tx Ants  :') then
         read(card,813) xmtrfile,design_freq,beam_main,TxPower
813      format(10x,1x,a21,1x,f7.3,f6.1,1x,f10.4)
      else if(card(1:10).eq.'Zones    :') then
         cirafz=card(11:40)       !  CIRAF zones to calculate
         platdeg=tlatdeg
         plondeg=tlondeg
         pname=tname
      end if
      go to 800
890   close(29)
c********************************************
      do 10 i=1,6
      layers(i)=-1
      ich=color(i)(1:1)
      if(ich.ge.'0' .and. ich.le.'9') layers(i)=ichar(ich)-ichar('0')
10    continue
      call gettra(tlat,tlon,plat,plon)   !  convert to decimal degrees
      do 20 nmonths=1,MAX_AREA_MONTHS    !  find out how many months
      if(montha(nmonths).eq.0) go to 25
20    continue
25    nmonths=nmonths-1
      if(meth.eq.'m') then
         nmonths=1                       !  only 1 plot for map only
         model='      '                  !  map only, no calculation model
      end if
c      nch=lcount(filename,30)
      grid_file=trim(root_directory)//data_dir//filename(1:nch-2)//'g?'
      nchg=lcount(grid_file,70)
      do 500 ii=1,MAX_AREA_MONTHS        !  create a file for each plot
c jw      call yieldit                   !  yield for windows control
      write(sufix,'(3h.da,i1)') ii
      call suffix(fileout,12,sufix,4)    !  append suffix
c jw      call erase@(fileout,error_code)!  delete file first
      call unlink(fileout,error_code)    !  delete file first
      grid_file(nchg:nchg)=sufix(4:4)
c jw      call erase@(grid_file,error_code)!  delete *.vg? files
      call unlink(grid_file,error_code)    !  delete *.vg? files
      if(ii.gt.nmonths) go to 500
c      open(29,file=run_directory(1:nch_run)//PATH_SEPARATOR//fileout)
      open(29,file=trim(run_directory)//PATH_SEPARATOR//fileout)
      rewind(29)
      write(29,1) model,filename
1     format('COMMENT   ',a6,4x,a)
      if(cirafz(1:10).eq.'          ') then     !  normal area
         write(29,2)   layers,cityname,
     +                 tlat,tlon,tname,beam_main,path,
     +                 plat,plon,
     +                 xmin,xmax,ymin,ymax,ngrid,ngrid,gridtype
2        format('COMMENT   ',6i5,1x,a,/,
     +          'COMMENT   ',2f10.4,1x,a20,1x,f6.1,1x,a,/
     +          'AREA      ',2f10.4,4f10.2,3i5)
         if(meth.eq.'m') go to 500        !  don't output model info
         write(29,3) nparms
3        format('COMMENT   Parameters:',i5)
         do 5 i=1,nparms
5        write(29,6) parms(i),nlevels(i),
     +              (ishades(j,i),conlevels(j,i),j=1,nlevels(i))
6        format('COMMENT   ',a,i4,10(i2,1h=,f7.2))
      else
         write(29,7)   tlat,tlon,tname,beam_main,path,
     +                 cirafz
7        format('COMMENT   ',2f10.4,1x,a20,1x,f6.1,1x,a,/
     +          'ZONES     ',a)
      end if
c******************************************************************
      if(model.eq.'VOACAP' .or. model.eq.'ICEPAC') then
c             Copy defaults from VOACAP.DEF
         open(32,file=trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'voacap.def',
     +        status='old',err=100)
90       read(32,'(a)',end=99) alf
         nch=lcount(alf,80)
         write(29,'(a)') alf(1:nch)
         go to 90
99       close(32)
      end if
c***************************************************************
100   write(29,'(10hCOEFFS    ,a)') coeffs
      write(29,101) ihour(ii),ihour(ii),1,1    !  always UT
101   format('TIME      ',4i5)
      if(model.eq.'ICEPAC' .or. model.eq.'VOACAP') then
         write(29,102) iyear,montha(ii)
102      format('MONTH     ',i5,f5.2)
      else
         write(29,103) iyear,ifix(montha(ii))
103      format('MONTH     ',2i5)
      end if

      if(model.eq.'VOACAP' .or. model.eq.'REC533') then
         write(29,104) float(ssna(ii))
104      format('SUNSPOT   ',f5.0,f5.2)
      else if(model.eq.'ICEPAC') then
         write(29,104) float(ssna(ii)),qindexa(ii)
      end if

      if(model.eq.'VOACAP') then
	 call getfreqs(Freq(ii),areafreqs,nfreqs)
         write(29,'(11hFREQUENCY $,11f6.3)') (areafreqs(i),i=1,nfreqs)
      else if(model.eq.'ICEPAC') then
	 call getfreqs(Freq(ii),areafreqs,nfreqs)
         write(29,'(10hFREQUENCY ,11f5.2)') (areafreqs(i),i=1,nfreqs)
      else
	 call getfreqs(Freq(ii),areafreqs,nfreqs)
         write(29,'(10hFREQ      ,10f7.3)') (areafreqs(i),i=1,nfreqs)
      end if

      tlatd=tlat
      tns='N'
      if(tlatd.lt.0.) tns='S'
      tlond=tlon
      if(tlond.gt.180.) tlond=tlond-360.
      tew='E'
      if(tlond.lt.0.) tew='W'
      platd=plat
      pns='N'
      if(platd.lt.0.) pns='S'
      plond=plon
      if(plond.gt.180.) plond=plond-360.
      pew='E'
      if(plond.lt.0.) pew='W'

      if(model.eq.'VOACAP' .or. model.eq.'ICEPAC') then
         write(29,105) tname,pname
105      format('LABEL     ',2a20)
         write(29,106) abs(tlatd),tns,abs(tlond),tew,
     +                 abs(platd),pns,abs(plond),pew,path(1:1),npsl
106      format('CIRCUIT   ',f5.2,a1,3(f9.2,a1),2x,a1,1x,i5)
      else if(model.eq.'REC533') then
         write(29,107) 'TRANS     ',tname,abs(tlatd),tns,abs(tlond),tew,
     +                              path(1:1)
         write(29,107) 'RECVR     ',pname,abs(platd),pns,abs(plond),pew,
     +                              path(1:1)
107      format(a10,a20,f7.2,a1,f10.2,a1,2x,a1,5x,1hD)
      end if

      xnoise=noise
      if(noise.eq.1) xnoise=-1.      !  =-140.4
      if(noise.eq.2) xnoise=-2.      !  =-144.7
      if(noise.eq.3) xnoise=-3.      !  =-150.0
      if(noise.eq.4) xnoise=-4.      !  =-164.1
      if(noise.eq.5) xnoise=-5.      !  =-138.7
      if(noise.eq.6) xnoise=-6.      !  =-152.7
      if(model.eq.'REC533') then
         write(29,216)1.,amind,0,0.
216      format('SYSTEM    ',f10.4,f5.2,i5,f5.1)
      else
         write(29,215)1.,xnoise,amind,float(xlufp),float(rsn),pmp,dmpx
215      format('SYSTEM    ',2f5.0,f5.2,f5.0,f5.1,2f5.2)
      end if

      if(model.eq.'REC533') then
         nnoise=noise
         if(nnoise.ge.1 .and. nnoise.le.6) nnoise=-nnoise  !  noise category
         write(29,217) -nnoise,bandw,rsn,xlufp
217      format('NOIS      ',4i5)
      end if

      if(model.eq.'VOACAP' .or. model.eq.'ICEPAC') then
      write(29,116) fprob
116   format('FPROB     ',4f5.2)
      end if

      call antfile_name(xmtrfile,system_type)
c          negative beam_main is for NON-terminated horizontal rhombics
      write(beam_alf,'(f5.1)') beam_main
      if(beam_alf(1:5).eq.'*****') write(beam_alf,'(f5.0)') beam_main
      write(29,117) 1,1,2,30,design_freq,xmtrfile,beam_alf,TxPower
117   format('ANTENNA   ',4i5,f10.3,1h[,a21,1h],a5,f10.4)
      call antfile_name(recfile,system_type)
      write(29,119) 2,2,2,30,0.,recfile,rec_bear,rec_gain
119   format('ANTENNA   ',4i5,f10.3,1h[,a21,1h],f5.1,f10.4)

      npage0=0
      write(29,118) 100+method,npage0
118   format('METHOD    ',2i5)

      write(29,'(7HEXECUTE)')

      write(29,'(4HQUIT)')
500   close(29)
      return
c***************************************************
900   write(*,901) ios,data_dir//filename
901   format('In areamap, err=',i5,' Could not OPEN file:',a)
      END
* -------------------------------------------------------------------- *
      subroutine getfreqs(Freq,areafreqs,nfreqs)
      use voacapl_defs
      use crun_directory
      dimension areafreqs(11)
      character filename*24
      areafreqs(1)=Freq
      nfreqs=1
      if(Freq.gt..5) return
c           get frequencies from areafreq.dat
c      nch_run=lcount(run_directory,50)
      open(41,file=trim(run_directory)//PATH_SEPARATOR//'areafreq.dat',
     +     status='old',err=100)
      filename=trim(run_directory)//PATH_SEPARATOR//'areafreq.dat'
      go to 110
100   open(41,file=trim(root_directory)//'database'//PATH_SEPARATOR//'areafreq.dat',
     +     status='old',err=200)
      filename=trim(root_directory)//'database'//PATH_SEPARATOR//'areafreq.dat'
110   rewind(41)
      read(41,*) areafreqs
      close(41)
      do 120 i=1,11
      if(areafreqs(i).gt.0.) nfreqs=i
120   continue
      write(*,121) filename,(areafreqs(i),i=1,nfreqs)
121   format(' Choosing MAXIMUM values from frequency file:',a,/
     +       ' Freqs=',11f6.2)
      write(*,122)
122   format(' TRANSMIT antenna MUST BE non-directional for this',
     +       ' purpose!')
200   return
      end
* -------------------------------------------------------------------- *
      subroutine antfile_name(file,system_type)
      character file*21,system_type*4,dir_bad*1,dir_good*1
      save dir_bad,dir_good
      if(system_type.eq.'DOS ') then
         dir_bad='/'
         dir_good='\'
      else
         dir_bad='\'
         dir_good='/'
      end if
      nch=21
      call rblankc(file,nch)     !  remove blanks
c jw      call lcase(file,nch)       !  convert to lower case
      idx=index(file,dir_bad)    !  change / or \ to for proper op sys
      if(idx.gt.0) file(idx:idx)=dir_good
      return
      end
* -------------------------------------------------------------------- *
      subroutine gettra(tlat,tlon,plat,plon)
      INCLUDE 'ficearea.hdr'
      character cdeg*10

      cdeg=tlatdeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,0,tlat,ierr)
      cdeg=tlondeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,1,tlon,ierr)
      cdeg=platdeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,0,plat,ierr)
      cdeg=plondeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,1,plon,ierr)
      return
      end
* -------------------------------------------------------------------- *
      SUBROUTINE col_FMT(lu,card,color,cityname)
c          read the COLORS & CITYNAME
c          handle both OLD & NEW formats
c          NEW format is compatible with HP UNIX format
      CHARACTER color(6)*6,cityname*12
      character card*80,ich*1,color_new(5)*6,shading*20
      character cities(6)*6,shads(7)*20
      character col_DOS*6
      data cities/'BLACK ','black ','RED   ','red   ','BLUE  ','blue  '/
      data shads/'Black with shading  ',
     +           'Red   with shading  ',
     +           'Black  NO  shading  ',
     +           'Red    NO  shading  ',
     +           'Shading, no contours',
     +           'Bin color shading   ',
     +           'Bin  B/W  shading   '/

      if(card(10:10).eq.':') then              !  new format
         read(card,2) color_new,shading
2        format(5(a6,4x),a20)
         color(1)=col_DOS(icolDOS(color_new(1)),1)    !  GRID
         color(2)=col_DOS(icolDOS(color_new(2)),1)    !  COUNTRIES
         color(3)=col_DOS(icolDOS(color_new(3)),1)    !  ZONES
         color(5)=col_DOS(icolDOS(color_new(5)),1)    !  MAINBEAM
c***********************************
c          CITIES (4) is different
      color(4)='Ignore'
      do 10 i=1,6
      if(color_new(4).eq.cities(i)) write(color(4),'(i1)') i-1
10    continue
      call colortab(4,color(4))
c***********************************
c          SHADING (6) is different
      color(6)='Ignore'
      do 20 i=1,7
      if(shading.eq.shads(i)) write(color(6),'(i1)') i-1
20    continue
      call colortab(6,color(6))
c***********************************
         read(lu,'(a)') card                   !  read cities file name
         nchc=lcount(card,80)
         do 30 i=nchc,1,-1
         ich=card(i:i)
         if(ich.eq.'/' .or. ich.eq.'\' .or. ich.eq.':') then
            cityname=card(i+1:nchc)
            go to 800
         end if
30       continue
      else                                     !  old format
         read (card,1) color,cityname
1        format(6(a6,3x),a)
      end if
800   if(cityname(1:1).eq.' ') cityname='receive.cty'
      return
      end
c-----------------------------------------------------------------
      subroutine colortab(idx,color)    !  set color from color table
      character color*6,colors(16)*6,ich*1,contours(7)*6,cities(6)*6
      data colors/'0=Blck','1=Red ','2=Grn ','3=Yell','4=Blue','5=Magn',
     +            '6=Cyan','7=Grey','8=DGry','9=Orng','A=Lgrn','B=Brwn',
     +            'C=Lblu','D=Pink','E=LLbl','F=Whit'/
      data contours/'0=BLCK','1=RED ','2=blck','3=red ','4=Shad',
     +              '5=Bin ','6=bin '/
      data cities/'0=BLCK','1=blck','2=RED ','3=red ','4=BLUE','5=blue'/
      ich=color(1:1)
      if(idx.eq.4) go to 50       !  Cities   are different
      if(idx.eq.6) go to 100      !  Contours are different
      do 10 i=1,16
      if(ich.ne.colors(i)(1:1)) go to 10
         color=colors(i)
         return
10    continue
      color='Ignore'
      return
50    do 60 i=1,6
      if(ich.ne.cities(i)(1:1)) go to 60
         color=cities(i)
         return
60    continue
      color='Ignore'
      return
100   do 110 i=1,7
      if(ich.ne.contours(i)(1:1)) go to 110
         color=contours(i)
         return
110   continue
      color='Ignore'
      return
      end
c-----------------------------------------------------------------
