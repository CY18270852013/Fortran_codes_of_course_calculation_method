program ThreeMethodsToCalculateLn2
    implicit none

    real(8) :: temp1 = 0,temp2 = 0, Sn = 0, Sn1 = 0, Sn2 = 0, Mn = 0, Tn = 0, k = 1
    integer :: count1 = 0, count2 = 0, count3 = 0
    do
      if (abs(0.69314718 - Sn) < 0.5E-5) exit
      temp1 = ((-1) ** (k - 1)) / k
      Sn2 = Sn1
      Sn1 = Sn
      Sn = temp1 + Sn
      k = k + 1
      count1 = count1 + 1
    end do
    write(*, *)"Sn =", Sn, "Es =", abs(log(2.0) - Sn), "   Iterations of Sn:", count1
    temp1 = 0
    Sn = 0
    Sn1 = 0
    Sn2 = 0
    Mn = 0
    k = 1

    do
      if (abs(0.69314718 - Mn) < 0.5E-5) exit
      temp1 = ((-1) ** (k - 1)) / k
      Sn2 = Sn1
      Sn1 = Sn
      Sn = temp1 + Sn
      Mn = Sn - ((Sn - Sn1) ** 2) / (Sn - 2 * Sn1 + Sn2)
      k = k + 1
      count2 = count2 + 1
    end do
    write(*, *)"Mn =", Mn, "Em =", abs(log(2.0) - Mn), "   Iterations of Mn:", count2
    temp2 = 0
    Tn = 0
    k = 1

    do
      if (abs(0.69314718 - Tn) < 0.5E-5) exit
      temp2 = 1 / (k * (2 ** k))
      Tn = Tn + temp2
      k = k + 1
      count3 = count3 + 1
    end do
    write(*, *)"Tn =", Tn, "Et =", abs(log(2.0) - Tn), "   Iterations of Tn:", count3

end program