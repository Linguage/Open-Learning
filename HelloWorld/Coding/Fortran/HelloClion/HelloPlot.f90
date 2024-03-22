module typedef
    type point
        real x,y,z
end typegfortran
contains
    subroutine set_p(p,x,y,z)
        type(point) p
        real x,y,z
        p.x=x;p.y=y;p.z=z
    end subroutine !Attention here
    subroutine show_p(P)
        type(point) P
        print 100,"  x=",P.x,"  y=",P.y,"  f(x,y)=",P.z
        100     format(a4,f9.3,a4,f9.3,a9,f9.3)
    end subroutine
end module


program Hello
    use typedef
    implicit none
    type(point) Q11,Q12,Q21,Q22,P  !Attention here
    real x,y

    call set_p(Q11,0.0,0.0,0.0)
    call set_p(Q21,1.0,0.0,1.0)
    call set_p(Q12,0.0,1.0,2.0)
    call set_p(Q22,1.0,1.0,3.0)
    p.x=0.3;p.y=0.7
    call sub(Q11,Q12,Q21,Q22,P)

    print *,"Q11:"
    call show_p(Q11)
    print *,"Q12:"
    call show_p(Q12)
    print *,"Q21:"
    call show_p(Q21)
    print *,"Q22:"
    call show_p(Q22)
    print *,"P:"
    call show_p(P)
    pause
end

subroutine sub(Q11,Q12,Q21,Q22,P)
    use typedef
    implicit none
    type(point) Q11,Q12,Q21,Q22,P,R1,R2,fun  !Attention here
    R1=fun(Q11,Q21,(P.x-Q11.x)/(Q21.x-Q11.x))
    R2=fun(Q12,Q22,(P.x-Q12.x)/(Q22.x-Q12.x))
    P=fun(R1,R2,(P.y-R1.y)/(R2.y-R1.y))
end subroutine

function fun(Q1,Q2,x) result(P)
    use typedef
    implicit none
    type(point) Q1,Q2,P
    real x  !x is PQ1/PQ2
    P.z=x*Q2.z+(1-x)*Q1.z
    P.x=x*Q2.x+(1-x)*Q1.x
    P.y=x*Q2.y+(1-x)*Q1.y
end
