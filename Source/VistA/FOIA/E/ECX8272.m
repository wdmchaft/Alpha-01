ECX8272 ; COMPILED XREF FOR FILE #727.827 ; 12/14/10
 ; 
 S DIKZK=1
 S DIKZ(0)=$G(^ECX(727.827,DA,0))
 S X=$P($G(DIKZ(0)),U,3)
 I X'="" S ^ECX(727.827,"AC",$E(X,1,30),DA)=""
END Q
