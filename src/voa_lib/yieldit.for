      subroutine yieldit     !  yield program control for Windows
      include <windows.ins>
      call yield_program_control(Y_TEMPORARILY)
      return
      end
