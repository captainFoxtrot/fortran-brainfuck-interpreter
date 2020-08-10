!    Copyright (C) 2020 Captain Foxtrot
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <https://www.gnu.org/licenses/>.

program bfInterpreter

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Declare variables

    ! Info of file to read
    integer :: codeFileUnit = 10
    character (len = 255) :: codeFileName
    character (len = :), allocatable :: codeFileContents

    ! Iterators/temp variables
    integer :: i, j
    integer (kind = 1) :: repeatCount = 0, loopDepth = 0
    character :: lastInstruction, currentInstruction
    logical :: isFirstCharacter = .true.

    ! Virtual machine
    integer (kind = 1), dimension(30000) :: memory
    integer (kind = 1), dimension(:), allocatable :: byteCode
    integer :: ptr = 1, byteCodePointer = 1
    integer (kind = 1) :: inputChar, jumpLabel
    character, dimension(8) :: codeChars = (/'+', '-', '>', '<', '[', ']', '.', ','/)

    real :: start, finish
    logical :: showTimer = .false.

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Read the code

    ! https://rosettacode.org/wiki/Read_entire_file#Fortran
    ! https://www.tutorialspoint.com/fortran/fortran_file_input_output.htm
    ! https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnat/index.html

    ! showTimer command line argument
    ! Displays the time it took to compile and run the program after the program has terminated.
    call get_command_argument(2, codeFileName)
    if (trim(codeFileName) == "showTimer") then
        showTimer = .true.
    end if

    ! Get the file name
    call get_command_argument(1, codeFileName)

    ! If no file entered
    if (trim(codeFileName) == '') then
        print *, "Enter a file to run."
        stop
    end if

    ! Open the file
    open( &
        unit = codeFileUnit, &
        file = trim(codeFileName), &
        action = "read", &
        form = "unformatted", &
        access = "stream" &
    )

    ! Get file size, store in j
    inquire(codeFileUnit, size = j)

    ! Allocate memory for its contents
    allocate(character(j) :: codeFileContents)

    ! Allocate bytecode
    allocate(byteCode(int(j * 2)))

    ! Store the file contents
    read(codeFileUnit) codeFileContents

    ! Close the file
    close(codeFileUnit)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Compile code using run-length encoding, with the exception of brackets

    if (showTimer) then
        call cpu_time(start)
    end if
    
    ! Loop through every single character in the code
    do i = 1, j

        ! Get the current character
        currentInstruction = codeFileContents(i:i)

        ! Skip non-code characters
        if (.not. any(codeChars == currentInstruction)) then
            cycle
        end if

        ! If we are on the first character then we need to skip the
        ! run-length encoding stuff because there was no character before
        ! this one to check if a run had ended.
        if (isFirstCharacter) then
            ! No more first characters...obviously
            isFirstCharacter = .false.

        ! Otherwise, if a new run has begun and the new character is not a
        ! loop control character (which are exempt from the run-length encoding)
        else if (lastInstruction /= currentInstruction .and. lastInstruction /= ']' .and. lastInstruction /= '[') then

            ! Add the last instruction
            byteCode(byteCodePointer) = int(findloc(codeChars, lastInstruction, dim = 1), 1)
            byteCodePointer = byteCodePointer + 1

            ! Add the amount of times that instruction was repeated
            byteCode(byteCodePointer) = repeatCount
            byteCodePointer = byteCodePointer + 1

            ! Reset repeat count
            repeatCount = 0
        end if

        if (currentInstruction == '[') then

            ! Add instruction
            byteCode(byteCodePointer) = 5
            byteCodePointer = byteCodePointer + 1
            
            ! Add loop depth
            byteCode(byteCodePointer) = loopDepth
            byteCodePointer = byteCodePointer + 1

            ! Increment loop depth
            loopDepth = int(loopDepth + 1, 1)

        else if (currentInstruction == ']') then

            ! Decrement loop depth
            loopDepth = int(loopDepth - 1, 1)

            ! Add instruction
            byteCode(byteCodePointer) = 6
            byteCodePointer = byteCodePointer + 1
            
            ! Add loop depth
            byteCode(byteCodePointer) = loopDepth
            byteCodePointer = byteCodePointer + 1

        else
            repeatCount = int(repeatCount + 1, 1)
        end if

        lastInstruction = currentInstruction

    end do

    ! Add instruction
    byteCode(byteCodePointer) = int(findloc(codeChars, lastInstruction, dim = 1), 1)
    byteCodePointer = byteCodePointer + 1

    ! Add repeat count
    byteCode(byteCodePointer) = repeatCount
    byteCodePointer = byteCodePointer + 1

    ! Add program end instruction
    byteCode(byteCodePointer) = 0
    byteCodePointer = byteCodePointer + 1
    byteCode(byteCodePointer) = 0

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Run code

    ! Initialize memory
    do i = 1, size(memory)
        memory(i) = 0
    end do

    i = 1
    do
        select case (byteCode(i))

            ! Terminate program
            case (0)

                call cpu_time(finish)
                if (showTimer) then
                    print *, (finish - start) * 1000, "ms"
                end if
                stop

            ! +
            case (1)
                memory(ptr) = int(memory(ptr) + byteCode(i + 1), 1)
                if (memory(ptr) > 255) then
                    memory(ptr) = int(memory(ptr) - 255, 1)
                end if

            ! -
            case (2)
                memory(ptr) = int(memory(ptr) - byteCode(i + 1), 1)
                if (memory(ptr) < 0) then
                    memory(ptr) = int(memory(ptr) + 255, 1)
                end if

            ! >
            case (3)
                ptr = ptr + byteCode(i + 1)
                if (ptr > 30000) then
                    ptr = ptr - 30000
                end if

            ! <
            case (4)
                ptr = ptr - byteCode(i + 1)
                if (ptr < 1) then
                    ptr = ptr + 30000
                end if

            ! [
            case (5)

                ! If the current cell is 0 then we need to skip this loop
                if (memory(ptr) == 0) then

                    jumpLabel = byteCode(i + 1)

                    do while(.not. (byteCode(i) == 6 .and. byteCode(i + 1) == jumpLabel))
                        i = i + 2
                    end do
                end if

            ! ]
            case (6)

                ! If the current cell is not 0 then we need to jump back to the beginning of the loop
                if (memory(ptr) /= 0) then

                    jumpLabel = byteCode(i + 1)

                    do while(.not. (byteCode(i) == 5 .and. byteCode(i + 1) == jumpLabel))
                        i = i - 2
                    end do

                end if

            ! .
            case (7)
                ! Repeat the character however many times was specified and write without a newline
                write (*, "(A)", advance="no") repeat(char(memory(ptr)), byteCode(i + 1))

            ! ,
            case (8)
                do i = 1, byteCode(i + 1)
                    read *, inputChar
                end do

        end select

        ! Move to the next instruction
        i = i + 2

    end do

end program bfInterpreter
