XDRMRG1 ;IHS/OHPRD/JCM - ERROR TRAP FOR XDRMRG; ;07/12/93  15:53
 ;;7.3;TOOLKIT;;Apr 25, 1995
MERGE ;
 S XDRMRG1("PKGE")=$P(^DIC(9.4,XDRMPKGE,0),U,1)
 D ERROR
 S X="MERGE^XDRMRG1",@^%ZOSF("TRAP")
 K X,XDRMRG1
 Q
 ;
DIT0 ;
 S XDRMRG1("NODIE")="",XDRMRG1("PKGE")="DIT0-File: "_XDRMRGFL
 D ERROR
 S X="DIT0^XDRMRG1",@^%ZOSF("TRAP")
 K X,XDRMRG1
 Q
 ;
DITMGMRG ;
 S XDRMRG1("NODIE")="",XDRMRG1("PKGE")="REPOINT-File: "_XDRMRGFL
 D ERROR
 S X="DITMGMRG^XDRMRG1",@^%ZOSF("TRAP")
 K X,XDRMRG1
 Q
 ;
DITM2 ;
 S XDRMRG1("NODIE")="",XDRMRG1("PKGE")="DITM2-File: "_XDRMRGFL
 D ERROR
 S X="DITM2^XDRMRG1",@^%ZOSF("TRAP")
 K X,XDRMRG1
 Q
 ;
DIK ;
 S XDRMRG1("NODIE")="",XDRMRG1("PKGE")="DIK-File: "_XDRMRGFL
 D ERROR
 S X="DIK^XDRMRG1",@^%ZOSF("TRAP")
 K X,XDRMRG1
 Q
 ;
ERROR ;
 S XDRMRG1("SPACE")=$J(" ",25)
 D:'$D(XDRMERR(1)) HEADER
 S XDRMERR(XDRMRG("ERRCNT"))="Package:  "_$E(XDRMRG1("PKGE")_XDRMRG1("SPACE"),1,25)_"Error: "_$$EC^%ZOSV
 D:'$D(XDRMRG1("NODIE")) DIE
 D @^%ZOSF("ERRTN")
 S XDRMRG("ERRCNT")=XDRMRG("ERRCNT")+1
 Q
HEADER ;
 F %=1:1:2 S XDRMERR(%)=" "
 K %
 S XDRMERR(3)="***** The following errors occured during the merge process *****"
 S XDRMERR(4)=" ",XDRMRG("ERRCNT")=5,XDRQFLG=1
 Q
 ;
DIE ;
 S %=$$EC^%ZOSV
 S DA(1)=XDRMPDA,DA=XDRMPKGE,DIE="^VA(15,"_DA(1)_",11,"
 S DR=".03////"_$P(%,U)_"**"_$P(%,U,2)
 D ^DIE K DA,DR,DIE
 Q
