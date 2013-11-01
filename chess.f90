MODULE GLOBALS
  ! Global variables:
  ! level = current recursion level for calculation
  ! maxlevel = maximum recursion level
  ! score = current score (evaluation)
  ! besta, bestb, bestx, besty = holds best moves for each recursion level
  ! wcksflag, wcqsflag = flags to detemine castling abilities
  ! board = the 8x8 array to hold chessboard
  INTEGER , PARAMETER :: MAXLEVEL = 5
  INTEGER LEVEL, SCORE, BOARD(0:7, 0:7), BESTA(0:7) 
  INTEGER BESTB(1:MAXLEVEL), BESTX(1:MAXLEVEL), BESTY(1:MAXLEVEL)
  LOGICAL WCKSFLAG, WCQSFLAG
END MODULE GLOBALS


PROGRAM CHESS 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z)
  ! initialize board to starting position
  BOARD = RESHAPE( (/ -500, -100, 0, 0, 0, 0, 100,  500,  & 
                      -270, -100, 0, 0, 0, 0, 100,  270,  & 
                      -300, -100, 0, 0, 0, 0, 100,  300,  & 
                      -900, -100, 0, 0, 0, 0, 100,  900,  & 
                     -7500, -100, 0, 0, 0, 0, 100, 5000,  & 
                      -300, -100, 0, 0, 0, 0, 100,  300,  & 
                      -270, -100, 0, 0, 0, 0, 100,  270,  & 
                      -500, -100, 0, 0, 0, 0, 100,  500  /), SHAPE(BOARD)) 
  LEVEL=0; A=-1; RES=0
  WCKSFLAG = .FALSE.; WCQSFLAG = .FALSE.
  ! main loop: get white move from user, calculate black move
  DO 
     SCORE=0 
     CALL IO(A, B, X, Y, RES) 
     RES=EVALUATE(-1, 10000) 
     A=BESTA(1); B=BESTB(1); X=BESTX(1); Y=BESTY(1) 
  END DO
END PROGRAM CHESS

! figure out if white is in check
FUNCTION INCHECK(ID) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26), CC(0:26)
  DO B = 0, 7 
     DO A = 0, 7 
        IF (SGN(BOARD(B, A)) /= -ID) CYCLE 
        CALL MOVELIST(A, B, XX, YY, CC, NDX)
        ! iterate through move list and see if
        ! piece can get to king
        DO I = 0, NDX, 1 
           X = XX(I) 
           Y = YY(I) 
           IF ((ID > 0 .AND. BOARD(Y, X) == 5000)  .OR. &
              (ID < 0 .AND. BOARD(Y, X) == -7500)) THEN
              INCHECK = 1
              RETURN 
           END IF
        END DO
     END DO
  END DO
  INCHECK = 0
  RETURN 
END FUNCTION INCHECK

RECURSIVE FUNCTION EVALUATE (ID, PRUNE) RESULT (RES) 
  USE GLOBALS 
  IMPLICIT INTEGER(A-Z) 
  DIMENSION XX(0:26), YY(0:26), CC(0:26)
  LEVEL=LEVEL+1 
  BESTSCORE=10000*ID
  NUMMOVES=0
  CHECKFLAG = INCHECK(ID)
  DO B=7,0, -1 
     DO A=7,0, -1
        ! generate the moves for all the pieces
        ! and iterate through them 
        IF (SGN(BOARD(B,A))/=ID) CYCLE 
        CALL MOVELIST (A, B, XX, YY, CC, NDX)
        NUMMOVES=NUMMOVES+NDX+1 
        DO I=0,NDX,1 
           X=XX(I); Y=YY(I); C=CC(I)
           OLDSCORE=SCORE; MOVER=BOARD(B,A); TARG=BOARD(Y,X)
           ! make the move and evaluate the new position
           ! recursively. Targ holds the relative value of the piece
           ! allowing use to calculate material gain/loss 
           CALL MAKEMOVE (A, B, X, Y, C)
	   IF (INCHECK(ID) /= 0) THEN
              SCORE=10000*ID
              NUMMOVES=NUMMOVES-1
           ELSE IF (LEVEL<MAXLEVEL) THEN
              SCORE=SCORE+EVALUATE(-ID, &
                    BESTSCORE-TARG+ID*(8-ABS(4-X)-ABS(4-Y)))
           END IF
           SCORE=SCORE+TARG-ID*(8-ABS(4-X)-ABS(4-Y))
           ! we want to minimize the maximum possible loss
           ! for black
           IF ((ID<0 .AND. SCORE>BESTSCORE) .OR. &
              (ID>0 .AND. SCORE<BESTSCORE)) THEN
              BESTA(LEVEL)=A; BESTB(LEVEL)=B 
              BESTX(LEVEL)=X; BESTY(LEVEL)=Y 
              BESTSCORE=SCORE 
              IF ((ID<0 .AND. BESTSCORE>=PRUNE) .OR. &
                 (ID>0 .AND. BESTSCORE<=PRUNE)) THEN
                 BOARD(B,A)=MOVER; BOARD(Y,X)=TARG; SCORE=OLDSCORE 
                 LEVEL=LEVEL-1 
                 RES = BESTSCORE 
                 RETURN 
              END IF
           END IF
           BOARD(B,A)=MOVER; BOARD(Y,X)=TARG; SCORE=OLDSCORE 
        END DO
     END DO
  END DO
  LEVEL=LEVEL-1
  IF ((NUMMOVES == 0) .AND. (CHECKFLAG /= 0)) THEN
    RES = 10000*ID ! Checkmate
  ELSE IF (NUMMOVES == 0) THEN
    RES = 0 ! Stalemate
  ELSE
    RES=BESTSCORE
  END IF
  RETURN 
END FUNCTION EVALUATE

! make a move given the start square and end square
! currently always promotes to queen
SUBROUTINE MAKEMOVE (A, B, X, Y, C) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  BOARD(Y, X)=BOARD(B, A); BOARD(B, A)=0 
  IF (Y == 0 .AND. BOARD(Y, X) ==  100) BOARD(Y, X)=  C
  IF (Y == 7 .AND. BOARD(Y, X) == -100) BOARD(Y, X)=  C
  RETURN 
END SUBROUTINE MAKEMOVE

! select appropriate subprogram to populate xx and yy arrays
! with piece moves
! xx = x coordinates
! yy = y coordinates
! cc = pawn promotion if applicable
! ndx = index into xx, yy, cc arrays showing the number of
!       elements that the arrays have been populated with
SUBROUTINE MOVELIST (A, B, XX, YY, CC, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26), CC(0:26)
  PIECE=ABS(BOARD(B, A)); NDX=-1 
  SELECT CASE (PIECE) 
  CASE (100) 
     CALL PAWN(A, B, XX, YY, CC, NDX) 
  CASE (270) 
     CALL KNIGHT(A, B, XX, YY, NDX) 
  CASE (300) 
     CALL BISHOP(A, B, XX, YY, NDX) 
  CASE (500) 
     CALL ROOK(A, B, XX, YY, NDX) 
  CASE (900) 
     CALL QUEEN(A, B, XX, YY, NDX) 
  CASE DEFAULT 
     CALL KING(A, B, XX, YY, NDX) 
  END SELECT
  RETURN 
END SUBROUTINE MOVELIST

! queen is a combination of rook and bishop
SUBROUTINE QUEEN (A, B, XX, YY, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z)
  DIMENSION XX(0:26), YY(0:26)
  CALL ROOK(A, B, XX, YY, NDX) 
  CALL BISHOP(A, B, XX, YY, NDX) 
  RETURN 
END SUBROUTINE QUEEN


SUBROUTINE KING (A, B, XX, YY, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26) 
  ID=SGN(BOARD(B, A))
  ! negative = left or up
  ! positive = right or down
  ! zero = no change
  DO DY=-1, 1
     IF (B+DY<0 .OR. B+DY>7) CYCLE 
     DO DX=-1, 1
        IF (A+DX<0 .OR. A+DX>7) CYCLE 
        IF (ID/=SGN(BOARD(B+DY,A+DX))) THEN 
           NDX=NDX+1; XX(NDX)=A+DX; YY(NDX)=B+DY 
        END IF
     END DO
  END DO
  RETURN 
END SUBROUTINE KING


SUBROUTINE PAWN (A, B, XX, YY, CC, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26), CC(0:26)
  ID = SGN(BOARD(B, A)) 
  IF (((A - 1) >= 0) .AND. ((A - 1) <= 7) .AND. &
     ((B - ID) >= 0) .AND. ((B - ID) <= 7)) THEN
     IF (SGN(BOARD((B - ID), (A - 1))) == -ID) THEN 
        IF (((ID<0) .AND. (B == 6)) .OR. ((ID>0) .AND. &
           (B == 1))) THEN
           CC(NDX+1) = 270*ID
           CC(NDX+2) = 300*ID
           CC(NDX+3) = 500*ID
           CC(NDX+4) = 900*ID
           DO I=1, 4
             NDX = NDX + 1 
             XX(NDX) = A - 1 
             YY(NDX) = B - ID
           END DO
        ELSE
          NDX = NDX + 1 
          XX(NDX) = A - 1 
          YY(NDX) = B - ID
        END IF
     END IF
  END IF
  IF (((A + 1) >= 0) .AND. ((A + 1) <= 7) .AND. ((B - ID) >= 0) &
     .AND. ((B - ID) <= 7)) THEN
     IF (SGN(BOARD((B - ID), (A + 1))) == -ID) THEN
        IF (((ID<0) .AND. (B == 6)) .OR. ((ID>0) .AND. &
           (B == 1))) THEN
           CC(NDX+1) = 270*ID
           CC(NDX+2) = 300*ID
           CC(NDX+3) = 500*ID
           CC(NDX+4) = 900*ID
           DO I=1, 4
             NDX = NDX + 1 
             XX(NDX) = A + 1 
             YY(NDX) = B - ID           
           END DO
        ELSE
          NDX = NDX + 1 
          XX(NDX) = A + 1 
          YY(NDX) = B - ID 
        END IF
     END IF
  END IF
  IF ((A >= 0) .AND. (A <= 7) .AND. ((B - ID) >= 0) .AND. &
     ((B - ID) <= 7)) THEN
     IF (BOARD((B - ID), A) == 0) THEN
        IF (((ID<0) .AND. (B == 6)) .OR. ((ID>0) .AND. &
           (B == 1))) THEN
          CC(NDX+1) = 270*ID
          CC(NDX+2) = 300*ID
          CC(NDX+3) = 500*ID
          CC(NDX+4) = 900*ID
          DO I=1, 4
            NDX = NDX + 1 
            XX(NDX) = A 
            YY(NDX) = B - ID 
          END DO
        ELSE
          NDX = NDX + 1 
          XX(NDX) = A 
          YY(NDX) = B - ID 
        END IF
        IF (((ID < 0) .AND. (B == 1)) .OR. ((ID > 0) .AND. &
           (B == 6))) THEN
           IF (BOARD((B - ID - ID), A) == 0) THEN 
              NDX = NDX + 1 
              XX(NDX) = A 
              YY(NDX) = B - ID - ID 
           END IF
        END IF
     END IF
  END IF
END SUBROUTINE PAWN


SUBROUTINE BISHOP (A, B, XX, YY, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26) 
  ID=SGN(BOARD(B, A))
! four diagonal directions
  DO DXY=1, 7 
     X=A-DXY; IF (X<0) EXIT 
     Y=B+DXY; IF (Y>7) EXIT 
     IF (ID/=SGN(BOARD(Y, X))) THEN ! cannot capture piece of same color
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y
     END IF
     IF (BOARD(Y, X)/=0) EXIT ! cannot jump over pieces
  END DO
  DO DXY=1, 7 
     X=A+DXY; IF (X>7) EXIT 
     Y=B+DXY; IF (Y>7) EXIT 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
     IF (BOARD(Y, X)/=0) EXIT 
  END DO
  DO DXY=1, 7 
     X=A-DXY; IF (X<0) EXIT 
     Y=B-DXY; IF (Y<0) EXIT 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
     IF (BOARD(Y, X)/=0) EXIT 
  END DO
  DO DXY=1, 7 
     X=A+DXY; IF (X>7) EXIT 
     Y=B-DXY; IF (Y<0) EXIT 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
     IF (BOARD(Y, X)/=0) EXIT 
  END DO
END SUBROUTINE BISHOP

SUBROUTINE ROOK (A, B, XX, YY, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26) 
  ID=SGN(BOARD(B, A))
! four different orthagonal directions 
  DO X = A-1, 0, -1 
     IF (ID/=SGN(BOARD(B, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=B 
     END IF
     IF (BOARD(B, X)/=0) EXIT 
  END DO
  DO X = A+1, 7, 1 
     IF (ID/=SGN(BOARD(B, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=B 
     END IF
     IF (BOARD(B, X)/=0) EXIT 
  END DO
  DO Y = B-1, 0, -1 
     IF (ID/=SGN(BOARD(Y, A))) THEN 
        NDX=NDX+1; XX(NDX)=A; YY(NDX)=Y 
     END IF
     IF (BOARD(Y, A)/=0) EXIT 
  END DO
  DO Y = B+1, 7, 1 
     IF (ID/=SGN(BOARD(Y, A))) THEN 
        NDX=NDX+1; XX(NDX)=A; YY(NDX)=Y 
     END IF
     IF (BOARD(Y, A)/=0) EXIT 
  END DO
  RETURN 
END SUBROUTINE ROOK


SUBROUTINE KNIGHT (A, B, XX, YY, NDX) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  DIMENSION XX(0:26), YY(0:26) 
  ID=SGN(BOARD(B, A))
! 2 vertical, 1 horizontal
! or 2 horizontal, 1 vertical
  X=A-1; Y=B-2 
  IF (X>=0 .AND. Y>=0) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A-2; Y=B-1 
  IF (X>=0 .AND. Y>=0) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A+1; Y=B-2 
  IF (X<=7 .AND. Y>=0) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A+2; Y=B-1 
  IF (X<=7 .AND. Y>=0) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A-1; Y=B+2 
  IF (X>=0 .AND. Y<=7) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A-2; Y=B+1 
  IF (X>=0 .AND. Y<=7) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A+1; Y=B+2 
  IF (X<=7 .AND. Y<=7) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  X=A+2; Y=B+1 
  IF (X<=7 .AND. Y<=7) THEN 
     IF (ID/=SGN(BOARD(Y, X))) THEN 
        NDX=NDX+1; XX(NDX)=X; YY(NDX)=Y 
     END IF
  END IF
  RETURN 
END SUBROUTINE KNIGHT

! display chessboard
SUBROUTINE SHOW 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z)
  DO B=0, 7
     WRITE (*, '(A)') '   +---+---+---+---+---+---+---+---+'
     WRITE (*, '(A)', ADVANCE='NO') ACHAR(56-B) // '  |' 
     DO A=0, 7
        SELECT CASE (BOARD(B, A)) 
        CASE (-7500) 
           WRITE (*, '(A)', ADVANCE='NO') ' *k|' 
        CASE (-900) 
           WRITE (*, '(A)', ADVANCE='NO') ' *q|' 
        CASE (-500) 
           WRITE (*, '(A)', ADVANCE='NO') ' *r|' 
        CASE (-300) 
           WRITE (*, '(A)', ADVANCE='NO') ' *b|' 
        CASE (-270) 
           WRITE (*, '(A)', ADVANCE='NO') ' *n|' 
        CASE (-100) 
           WRITE (*, '(A)', ADVANCE='NO') ' *p|' 
        CASE (-0) 
           WRITE (*, '(A)', ADVANCE='NO') '   |' 
        CASE (100) 
           WRITE (*, '(A)', ADVANCE='NO') ' P |' 
        CASE (270) 
           WRITE (*, '(A)', ADVANCE='NO') ' N |' 
        CASE (300) 
           WRITE (*, '(A)', ADVANCE='NO') ' B |' 
        CASE (500) 
           WRITE (*, '(A)', ADVANCE='NO') ' R |' 
        CASE (900) 
           WRITE (*, '(A)', ADVANCE='NO') ' Q |' 
        CASE (5000) 
           WRITE (*, '(A)', ADVANCE='NO') ' K |' 
        END SELECT
     END DO
     WRITE (*, *) '' 
  END DO
  WRITE (*, '(A)') '   +---+---+---+---+---+---+---+---+'
  WRITE (*, '(A)') '     A   B   C   D   E   F   G   H'
  RETURN 
END SUBROUTINE SHOW

! io -- input/output:
! display black move and get white move
SUBROUTINE IO (A, B, X, Y, RES) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  CHARACTER (LEN=10) INPUT
  LOGICAL WCKSOLD, WCQSOLD 
  DIMENSION XX(0:26), YY(0:26), CC(0:26) 
  IF (A>=0) THEN 
     IF (RES<-2500) THEN 
        PRINT *, 'I RESIGN' 
        CALL SHOW 
        WRITE (*, *) 
        STOP 
     END IF
     PIECE=BOARD(Y, X) 
     CALL MAKEMOVE(A, B, X, Y, C) 
     WRITE (*, '(A)', ADVANCE='NO') 'MY MOVE:  ' 
     WRITE (*, '(A)') ACHAR(65+A) // ACHAR(56-B) // '-' // ACHAR(65+X) // &
                      ACHAR(56-Y)
     SELECT CASE (PIECE) 
     CASE (100) 
        PRINT *, 'I TOOK YOUR PAWN' 
     CASE (270) 
        PRINT *, 'I TOOK YOUR KNIGHT' 
     CASE (300) 
        PRINT *, 'I TOOK YOUR BISHOP' 
     CASE (500) 
        PRINT *, 'I TOOK YOUR ROOK' 
     CASE (900) 
        PRINT *, 'I TOOK YOUR QUEEN' 
     CASE (5000) 
        PRINT *, 'I TOOK YOUR KING' 
     END SELECT
  END IF
  DO 
     CALL SHOW 
     WRITE (*, '(A)', ADVANCE='NO') 'YOUR MOVE:  ' 
     READ (*, *) INPUT 
     CALL UPCASE(INPUT) 
     IF ((INPUT == 'QUIT') .OR. (INPUT == 'BYE') .OR. (INPUT == 'EXIT')) THEN 
        STOP 
     END IF
! castling
     IF ((INPUT == 'O-O') .OR. (INPUT == '0-0')) THEN 
        IF (INCHECK(1) /= 0) CYCLE ! cannot castle out of check
        IF (WCKSFLAG) CYCLE
        IF (BOARD(7, 7) /= 500) CYCLE 
        IF ((BOARD (7,6) /= 0) .OR. (BOARD(7,5) /=0)) CYCLE
        BOARD(7, 4) = 0
        BOARD(7, 5) = 5000
        IF (INCHECK(1) /= 0) THEN ! cannot castle through check
          BOARD(7, 4) = 5000
          BOARD(7, 5) = 0
          CYCLE
        ELSE
          BOARD(7, 4) = 5000
          BOARD(7, 5) = 0
        END IF
        BOARD(7, 6) = 5000 
        BOARD(7, 4) = 0 
        BOARD(7, 5) = 500 
        BOARD(7, 7) = 0 
        IF (INCHECK(1) /= 0) THEN ! cannot castle into check
          BOARD(7, 6) = 0 
          BOARD(7, 4) = 5000 
          BOARD(7, 5) = 0 
          BOARD(7, 7) = 500 
          CYCLE
        ELSE
          WCKSFLAG = .TRUE.
          WCQSFLAG = .TRUE.       
          RETURN
        END IF 
     END IF
     IF ((INPUT == 'O-O-O') .OR. (INPUT == '0-0-0')) THEN
        IF (INCHECK(1) /= 0) CYCLE ! cannot castle out of check
        IF (WCQSFLAG) CYCLE 
        IF (BOARD(7,0) /= 500) CYCLE 
        IF ((BOARD(7,1) /= 0) .OR. (BOARD(7,2) /= 0) .OR. &
           (BOARD(7,3) /= 0)) CYCLE
        BOARD(7, 4) = 0
        BOARD(7, 3) = 5000
        IF (INCHECK(1) /= 0) THEN ! cannot castle through check
          BOARD(7, 4) = 5000
          BOARD(7, 3) = 0
          CYCLE
        ELSE
          BOARD(7, 4) = 5000
          BOARD(7, 3) = 0
        END IF
        BOARD(7, 2) = 5000 
        BOARD(7, 4) = 0 
        BOARD(7, 3) = 500 
        BOARD(7, 0) = 0 
        IF (INCHECK(1) /= 0) THEN ! cannot castle into check
          BOARD(7, 2) = 0 
          BOARD(7, 4) = 5000 
          BOARD(7, 3) = 0 
          BOARD(7, 0) = 500
          CYCLE
        ELSE
          WCKSFLAG = .TRUE.
          WCQSFLAG = .TRUE.       
          RETURN
        END IF 
     END IF
     B = 8 - (IACHAR(INPUT(2:2)) - 48) 
     A = IACHAR(INPUT(1:1)) - 65 
     X = IACHAR(INPUT(4:4)) - 65 
     Y = 8 - (IACHAR(INPUT(5:5)) - 48) 
     IF ((B>7) .OR. (B<0) .OR. (A>7) .OR. (A<0) .OR. (X>7) .OR. (X<0) .OR. &
        (Y>7) .OR. (Y<0)) CYCLE
     IF (BOARD(B,A)<=0) CYCLE
! en passant capture
     IF ((Y == 2) .AND. (B == 3) .AND. ((X == A-1) .OR. (X == A+1))) THEN
       IF ((BOARD(B,A) == 100) .AND. (BOARD(Y,X) == 0) &
          .AND. (BOARD(Y+1,X) ==-100)) THEN
         IF ((BESTB(1) == 1) .AND. (BESTA(1) == X)) THEN
           MOVER = BOARD(B,A)
           TARG = BOARD(Y,X)
           CALL MAKEMOVE(A,B,X,Y,C)
           BOARD(Y+1,X)=0
           IF ((INCHECK(1)) == 0) RETURN
           BOARD(B,A) = MOVER
           BOARD(Y, X) = TARG
           BOARD(Y+1,X) = -100
           CYCLE
         END IF
       END IF
     END IF                                         
     ! check if selected white move is on list of moves
     CALL MOVELIST(A, B, XX, YY, CC, NDX)
     DO K = 0, NDX, 1 
        IF ((X == XX(K)) .AND. (Y == YY(K))) THEN 
           MOVER = BOARD(B, A) 
           TARG = BOARD(Y, X)
           IF ((Y == 0) .AND. (MOVER == 100)) THEN
             DO
               WRITE (*, '(A)', ADVANCE='NO') 'PROMOTION PIECE: '
               READ (*, *) INPUT
               CALL UPCASE(INPUT)
               SELECT CASE (INPUT) 
                 CASE ('N', 'KT', 'KNIGHT', 'HORSE') 
                   C = 270 
                 CASE ('B', 'BISHOP') 
                   C = 300 
                 CASE ('R', 'ROOK') 
                   C = 500 
                 CASE ('Q', 'QUEEN') 
                   C = 900 
                 CASE DEFAULT 
                   CYCLE 
               END SELECT
               EXIT
             END DO
           END IF
           CALL MAKEMOVE(A, B, X, Y, C)
           IF (MOVER == 5000) THEN
             WCQSOLD = WCQSFLAG
             WCKSOLD = WCKSFLAG
             WCKSFLAG = .TRUE.
             WCQSFLAG = .TRUE.
           END IF
           IF ((A == 0) .AND. (B == 7) .AND. (MOVER == 500)) THEN
             WCQSOLD = WCQSFLAG
             WCQSFLAG = .TRUE.
           END IF
           IF ((A == 7) .AND. (B == 7) .AND. (MOVER == 500)) THEN
             WCKSOLD = WCKSFLAG
             WCKSFLAG = .TRUE.
           END IF
           IF (INCHECK(1) == 0) RETURN 
           BOARD(B, A) = MOVER 
           BOARD(Y, X) = TARG
           IF (MOVER == 5000) THEN
             WCQSFLAG = WCQSOLD
             WCKSFLAG = WCKSOLD
           END IF
           IF ((A == 0) .AND. (B == 7) .AND. (MOVER == 500)) THEN
             WCQSFLAG = WCQSOLD
           END IF
           IF ((A == 7) .AND. (B == 7) .AND. (MOVER == 500)) THEN
             WCKSFLAG = WCKSOLD
           END IF 
           EXIT 
        END IF
     END DO
  END DO
END SUBROUTINE IO

! get sign of number. a piece's 
! represents it's piece color
FUNCTION SGN(N) 
  USE GLOBALS 
  IMPLICIT INTEGER (A-Z) 
  IF (N < 0) THEN 
     SGN = -1 
  ELSE IF (N == 0) THEN 
     SGN = 0 
  ELSE 
     SGN = 1 
  END IF
END FUNCTION SGN

! convert string to uppercase
SUBROUTINE UPCASE (STRING) 
  USE GLOBALS
  IMPLICIT INTEGER (A-Z) 
  CHARACTER (LEN = *) STRING 
  LS = LEN(STRING) 
  DO LC = 1, LS 
     IF (LGE(STRING(LC:LC), 'a') .AND. LLE(STRING(LC:LC), 'z')) THEN 
        STRING(LC:LC) = ACHAR(IACHAR(STRING(LC:LC))-32) 
     END IF
  END DO
  RETURN 
END SUBROUTINE UPCASE




