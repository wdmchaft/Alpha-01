DGBTUTL ;ALB/SCK - BENEFICIARY/TRAVEL UTILITY ROUTINES; 1/6/93@1130
 ;;1.0;Beneficiary Travel;;September 25, 2001
START ;
 Q
MILES(DGBTRN,DGBTDX) ;
 ; DGBTRN holds the record no., and DGBTDX holds the division pointer passed in during the function call
 N DGBTML,XX,DGBTCHK
 S XX="",(DGBTML,DGBTDEF)=0
 F XX=0:0 S XX=$O(^DGBT(392.1,DGBTRN,1,XX)) Q:+XX'>0!(DGBTML>0)  D
 . S DGBTCHK=$P($G(^DGBT(392.1,DGBTRN,1,XX,0)),U,1) I DGBTDX=DGBTCHK S DGBTML=$P($G(^(0)),U,2)
 I DGBTML'>0 S DGBTML=$P($G(^DGBT(392.1,DGBTRN,0)),U,3),DGBTDEF=1
 K DGBTRN,DGBTDX
 Q DGBTML
DICLKUP(DGBTRN,DGBTDX,DGBTP) ;
 N RETURN,XX
 S DIC="^DGBT(392.1,DGBTRN,1,",DIC(0)="MZX",X=DGBTDX,RETURN=""
 D ^DIC
 I +Y>0 D
 . I DGBTP=4 S RETURN=$S(+$P($G(Y(0)),U,4)>0:$P($G(Y(0)),U,5),1:"")
 . I DGBTP=3 S RETURN=$S(+$P($G(Y(0)),U,3)>0:$P(^(0),U,3),1:0)
 Q RETURN
DEPCTY(ZIPCDE) ;
 N RETURN
 S DIC="^DGBT(392.1,",DIC(0)="MZ",X=$S($L(ZIPCDE)>5:$E(ZIPCDE,1,5),1:ZIPCDE) D ^DIC S RETURN=Y K DIC
 K ZIPCDE
 Q RETURN
EXIT ;
 Q
TEST ;
 W !,"DATE/TIME REQUIRED.."
 S X="OLD",DTOUT=1
 Q
