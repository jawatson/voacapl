      SUBROUTINE antmodel(iant,gain,ant_model)
* --------------------------------------------------------------------- *
      CHARACTER ant_model*10

      if(iant.eq.0) then    !  add gain above ISOTROPE
         write(ant_model,99) gain
99       format(1h+,f5.1,4h dBi)
      else if(iant.ge.1 .and. iant.le.10) then
         write(ant_model,'(8hREC705 #,i2.2)') iant
      else if(iant.eq.11) then
         ant_model='2-D Table '
      else if(iant.eq.12) then
         ant_model='NTIA87-215'
      else if(iant.eq.13) then
         ant_model='3-D Table '
      else if(iant.eq.14) then
         ant_model='2-D P-to-P'
      else if(iant.ge.21 .and. iant.le.30) then
         write(ant_model,'(8hIONCAP #,i2.2)') iant
      else if(iant.ge.31 .and. iant.le.47) then
         write(ant_model,'(8hHFMUFES#,i2.2)') iant
      else if(iant.ge.48 .and. iant.le.48) then
         write(ant_model,'(8hNOSC-95#,i2.2)') iant
      else if(iant.ge.90 .and. iant.le.99) then
         write(ant_model,'(8hExtern #,i2.2)') iant
      else
         ant_model='Unknown???'
      end if
      return
      end
