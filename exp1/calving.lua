function IfThenElse(condition,t,f)
   if condition then
     return t
   else
     return f
   end
end

function lsetcond(x, y, time, mask, velo)
  if (mask < 0.0) then
    radius = math.sqrt(x*x + y*y) + 0.0001
    ls = (velo + (300 * math.sin(2.0 * math.pi * time / 1000.0))) * x / radius
  else  
    ls = 0.0
  end
  return ls
end


function lsetvelo(x, y, time, mask, velo)
  if (mask < 0.0) then
    radius = math.sqrt(x*x + y*y) + 0.0001
    if (radius <= 75000.0001) then       
       lsvelo = 300.0 * math.sin(2.0 * math.pi * time / 1000.0) * x / radius
    else
       lsvelo = 0.0
    end   
  else
    lsvelo = 0.0
  end
  return lsvelo
end