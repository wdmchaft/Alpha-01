DICATTD7 ;SFISC/GFT-POINTERS ;03:29 PM  15 Dec 1998
 ;;22.0;VA FileMan;;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
POST7 ;
 N F
 S F=$$G(84)
 S DICATTMN=$G(^DD(DICATTA,DICATTF,3))
 S DICATT2N="P",DICATT5N="Q",DICATTLN=9,DICATT2N="P"_F_$E("'",'$$G(85))
 I F,$D(^DIC(F,0,"GL")) S DICATT3N=$P(^("GL"),U,2)
BRANCH I '$D(DICATTSC),DUZ(0)="@" S DICATTSC=7,DDSBR="65^DICATT SCREEN^6" Q
 D SCREEN^DICATTD3
 I $G(DICATT5N(12.1))]"" S DICATT5N=DICATT5N(12.1)_" D ^DIC K DIC S DIC=$G(DIE),X=+Y K:Y<0 X"
 Q
 ;
G(I) Q $$GET^DDSVALF(I,"DICATT7",2.7,"I","")
