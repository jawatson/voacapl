      function xlimit6(x,i)
c          make sure x will print in the F6.i format
      dimension xmin(4),xmax(4)
      data xmin/-999.9,-99.99,-9.999,-.9999/
      data xmax/9999.9,999.99,99.999,9.9999/
      xlimit6=x
      if(x.lt.xmin(i)) xlimit6=xmin(i)
      if(x.gt.xmax(i)) xlimit6=xmax(i)
      return
      end
