YSXRBA1 ; COMPILED XREF FOR FILE #628 ; 10/15/04
 ; 
 S DIKZK=2
 S DIKZ(0)=$G(^YS(628,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^YS(628,"B",$E(X,1,30),DA)
END G ^YSXRBA2
