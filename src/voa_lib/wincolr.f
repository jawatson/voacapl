c# WINcolr.for
      subroutine WINcolr    !  read DOS color table
      common /WIN_col/ ncolors,DOS_colors(20),colors(20),
     +            ncities_colors,DOS_cities(7),cities_colors(7),
     +            ncontours_colors,DOS_contours(8),contours_colors(8)
         character DOS_colors*6,colors*10
         character DOS_cities*6,cities_colors*20
         character DOS_contours*6,contours_colors*20
      character skip*1
      common /crun_directory/ run_directory
         character run_directory*50
      nch_run=lcount(run_directory,50)

      open(17,file=run_directory(1:nch_run-3)//'database/colors.win',
     +      status='old',err=900)
      rewind(17)
      read(17,'(a)') skip    !  skip header card
      read(17,*) ncolors,ncities_colors,ncontours_colors
      do 10 i=1,ncolors
10    read(17,11) DOS_colors(i),colors(i)
11    format(a6,1x,a)
      do 20 i=1,ncities_colors
20    read(17,11) DOS_cities(i),cities_colors(i)
      do 30 i=1,ncontours_colors
30    read(17,11) DOS_contours(i),contours_colors(i)
      close(17)
      return
900   write(*,'('' Could not OPEN file='',a,1h;)') 
     +     run_directory(1:nch_run-3)//'database/colors.win'
      stop 'Missing file ../database/colors.win in WINcolr'
      end
