program makeprime
    use fmzm
    implicit none

    ! Small primes table
    integer :: num_primes = 99
    integer, dimension(99) :: small_primes = [3, 5, 7, 11, 13, 17, 19, 23, 29, &
                    31, 37, 41, 43, 47, 53, 59, 61, 67, 71, &
                    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, &
                    127, 131, 137, 139, 149, 151, 157, 163, 167, 173, &
                    179, 181, 191, 193, 197, 199, 211, 223, 227, 229, &
                    233, 239, 241, 251, 257, 263, 269, 271, 277, 281, &
                    283, 293, 307, 311, 313, 317, 331, 337, 347, 349, &
                    353, 359, 367, 373, 379, 383, 389, 397, 401, 409, &
                    419, 421, 431, 433, 439, 443, 449, 457, 461, 463, &
                    467, 479, 487, 491, 499, 503, 509, 521, 523, 541]
    type(IM) :: candidate

    candidate = 10
    print *, '10:', divisible_by_small_primes(candidate)
    candidate = 10007
    print *, '10007:', divisible_by_small_primes(candidate)
    candidate = generate_candidate(100,.true.)
    print *, adjustl(im_format('i200',candidate))

    contains
        ! Divisible by small primes
        function divisible_by_small_primes(candidate) result (res)
            logical :: res
            integer :: i
            type(IM) :: candidate

            res = .false.
            do i = 1, num_primes
                if (mod(candidate, to_im(small_primes(i))) == 0) then
                    res = .true.
                end if
            end do
        end function divisible_by_small_primes

        ! Generate Candidate
        function generate_candidate(digits, want_random) result (res)
            integer :: digits
            double precision :: rand
            logical :: want_random
            type(IM) :: res

            res = (to_im(10)**to_im(digits-1)) + 1

            if (want_random) then
                call random_number(rand)
                res = (to_im(10)**to_im(digits)) * rand
                if (mod(res,to_im(2)) == 0) then
                    res = res + 1
                end if
            end if

            do while(divisible_by_small_primes(res))
                res = res + 2
            end do
        end function generate_candidate

        ! Miller Rabin
        function miller_rabin(n, k) result (res)
            integer :: k, s, i, j
            type(IM) :: n, d, a, x
            double precision :: rand
            logical :: res

            if (n < 2 .or. (n /= 2 .and. mod(n, to_im(2)) == 0)) then
                res = .false.
                return
            end if

            if (n < 6) then
                res = .true.
                return
            end if

            s = 0
            d = n - 1
            do while (mod(d, to_im(2)) == 0)
                d = d/2
                s = s + 1
            end do

            do i = 1, k
                call random_number(rand)
                a = 2 + (rand * (n-2))
                x = power_mod(a, d, n)
                if (x == 1 .or. x == n-1) continue
                do j = 1, s-1
                    x = power_mod(x, to_im(2), n)
                    if (x == n-1) then
                        exit
                    else
                        res = .false.
                        return
                    end if
                end do
            end do
            res = .true.
        end function miller_rabin

        ! Find Prime
        function find_prime(digits, want_twin, want_random) result (res)
            integer :: digits
            logical :: want_twin, want_random, found
            type(IM) :: res, candidate

            found = .false.
            candidate = generate_candidate(digits, want_random)
            do while (found .eqv. .false.)
                if (want_twin .eqv. .false. .and. divisible_by_small_primes(candidate) .eqv. .false. .and. miller_rabin(candidate, 10)) then
                    found = .true.
                else if (want_twin .eqv. .true. .and. divisible_by_small_primes(candidate) .eqv. .false. .and. miller_rabin(candidate, 10) .and. miller_rabin(candidate+2, 10)) then
                    found = .true.
                else
                    candidate = candidate + 2
                end if
            end do
            res = candidate
        end function find_prime

end program makeprime