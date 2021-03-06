OCXDI01J ;SLC/RJS,CLA - OCX PACKAGE DIAGNOSTIC ROUTINES ;SEP 7,1999 at 10:30
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**32**;Dec 17,1997
 ;;  ;;ORDER CHECK EXPERT version 1.01 released OCT 29,1998
 ;
S ;
 ;
 D DOT^OCXDIAG
 ;
 ;
 K REMOTE,LOCAL,OPCODE,REF
 F LINE=1:1:500 S TEXT=$P($T(DATA+LINE),";",2,999) Q:TEXT  I $L(TEXT) D  Q:QUIT
 .S ^TMP("OCXDIAG",$J,$O(^TMP("OCXDIAG",$J,"A"),-1)+1)=TEXT
 ;
 G ^OCXDI01K
 ;
 Q
 ;
DATA ;
 ;
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON LOW VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;R^"863.7:","863.74:3",.01,"E"
 ;;D^COMPARISON HIGH VALUE
 ;;R^"863.7:","863.74:3",1.1,"E"
 ;;D^3
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT LENGTH IS GREATER THAN
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT LENGTH IS GREATER THAN
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^LGRT^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT LENGTH IS LESS THAN
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT LENGTH IS LESS THAN
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^LESS^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT LENGTH IS INCLUSIVELY BETWEEN
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT LENGTH IS INCLUSIVELY BETWEEN
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^LINCL^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON LOW VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;R^"863.7:","863.74:3",.01,"E"
 ;;D^COMPARISON HIGH VALUE
 ;;R^"863.7:","863.74:3",1.1,"E"
 ;;D^3
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT LENGTH IS EXCLUSIVELY BETWEEN
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT LENGTH IS EXCLUSIVELY BETWEEN
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^LEXCL^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON LOW VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;R^"863.7:","863.74:3",.01,"E"
 ;;D^COMPARISON HIGH VALUE
 ;;R^"863.7:","863.74:3",1.1,"E"
 ;;D^3
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT NOT EQUALS
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT NOT EQUALS
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^NAEQ^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;EOR^
 ;;KEY^863.7:^GCC BOOLEAN LOGICAL TRUE
 ;;R^"863.7:",.01,"E"
 ;;D^GCC BOOLEAN LOGICAL TRUE
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^TRUE^OCXF23
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;EOR^
 ;;KEY^863.7:^GCC BOOLEAN LOGICAL FALSE
 ;;R^"863.7:",.01,"E"
 ;;D^GCC BOOLEAN LOGICAL FALSE
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^FALSE^OCXF23
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT EQUALS ELEMENT IN SET
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT EQUALS ELEMENT IN SET
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^EQSET^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT CONTAINS ELEMENT IN SET
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT CONTAINS ELEMENT IN SET
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^CONSET^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;EOR^
 ;;KEY^863.7:^GCC FREE TEXT TERM EQUALS
 ;;R^"863.7:",.01,"E"
 ;;D^GCC FREE TEXT TERM EQUALS
 ;;R^"863.7:",.02,"E"
 ;;D^EXTRINSIC FUNCTION
 ;;R^"863.7:",3,"E"
 ;;D^AEQT^OCXF22
 ;;R^"863.7:","863.74:1",.01,"E"
 ;;D^PRIMARY DATA FIELD
 ;;R^"863.7:","863.74:1",1.1,"E"
 ;;D^1
 ;;R^"863.7:","863.74:2",.01,"E"
 ;;D^COMPARISON VALUE
 ;;R^"863.7:","863.74:2",1.1,"E"
 ;;D^2
 ;;EOR^
 ;;EOF^OCXS(863.7)^1
 ;;SOF^863.9  OCX MDD CONDITION/FUNCTION
 ;;KEY^863.9:^BEFORE
 ;;R^"863.9:",.01,"E"
 ;;D^BEFORE
 ;;EOR^
 ;;KEY^863.9:^AFTER
 ;;R^"863.9:",.01,"E"
 ;;D^AFTER
 ;;R^"863.9:",.02,"E"
 ;;D^DATE/TIME
 ;;R^"863.9:",.03,"E"
 ;;D^TEST
 ;;R^"863.9:",.04,"E"
 ;;D^IS AFTER
 ;;R^"863.9:","863.91:3",.01,"E"
 ;;D^OCXO GENERATE CODE FUNCTION
 ;;R^"863.9:","863.91:3",1,"E"
 ;;D^GCC DATE/TIME AFTER
 ;;R^"863.9:","863.92:1",.01,"E"
 ;;D^AFTER
 ;;R^"863.9:","863.92:2",.01,"E"
 ;;D^GREATER THAN
 ;;R^"863.9:","863.92:3",.01,"E"
 ;;D^SINCE
 ;;R^"863.9:","863.92:4",.01,"E"
 ;;D^>
 ;;EOR^
 ;;KEY^863.9:^BETWEEN
 ;;R^"863.9:",.01,"E"
 ;1;
 ;
