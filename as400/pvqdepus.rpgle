      *============================================================================================*
      *                                                                            SyazwanH/030718 *
      * PVQDEPUS                                                                                   *
      * Monitor queue depth.                                                                       *
      *                                                                                            *
      * Version 1 Release 2 Modification 4                                                         *
      *                                                                                            *
      *  Compile as module. Then CRTPGM PGM(PVQDEPUS) MODULE(PVQDEPUS)                             *
      *   ACTGRP(*CALLER) ALWLIBUPD(*YES)                                                          *
      *                                                                                            *
      *============================================================================================*
      * Maintenance Log                                                                            *
      * ---------------                                                                            *
      * Trace  Date      Pgmr.     Notes                                                           *
      * ------------------------------------------------------------------------------------------ *
      *        20030711  SyazwanH  New.                                                            *
      *============================================================================================*
      *---------------------------------------------------------------------------------------------
      * Compile Options
      *---------------------------------------------------------------------------------------------
      *
     H AltSeq(*None) CvtOpt(*VarChar) Optimize(*Full) BndDir('QSNAPI')
      *
      *---------------------------------------------------------------------------------------------
      * Files Declaration
      *---------------------------------------------------------------------------------------------
      *
     FPvqdepuspfIF   E           K Disk
     FPvqdepusd CF   E             WorkStn Sfile(Sfl:Arrn) UsrOpn
      *
      *---------------------------------------------------------------------------------------------
      * Prototype Declarations
      *---------------------------------------------------------------------------------------------
      *
     D MakeBeep        Pr            10I 0 ExtProc('QsnBeep')
     D  CmdBuffHndl                  10I 0
     D  LowEnvmHndl                  10I 0
     D  ErrorCode                          Like(Qusec)
      *
     D GetCurAddr      Pr            10I 0 ExtProc('QsnGetCsrAdr')
     D  CurRow                       10I 0
     D  CurCol                       10I 0
     D  LowEnvmHndl                  10I 0
     D  ErrorCode                          Like(Qusec)
      *
      *---------------------------------------------------------------------------------------------
      * System Data Structure
      *---------------------------------------------------------------------------------------------
      *
     D                SDs
     D  QMsgTyp               40     42                                         Message type
     D  QMsgNbr               43     46                                         Message number
      *
      *---------------------------------------------------------------------------------------------
      * Local Data Structure
      *---------------------------------------------------------------------------------------------
      *
     D UserSpace       Ds            20    Qualified
     D  Name                   1     10A   Inz('PVQDEPUS')
     D  Library               11     20A   Inz('QTEMP')
      *
     D ObjectPath      Ds            20    Qualified
     D  Name                   1     10A   Inz('*ALL')
     D  Library               11     20A   Inz(*Blanks)
      *
     D ObjectInfo      Ds            20    Qualified Occurs(9999)
     D  Name                   1     10A   Inz(*Blanks)
     D  Library               11     20A   Inz(*Blanks)
      *
     D ObjectDir       Ds            20    Qualified
     D  Name                   1     10A   Inz(*Blanks)
     D  Library               11     20A   Inz(*Blanks)
      *
     D QueuePath       Ds            20    Qualified
     D  Name                   1     10A   Inz(*Blanks)
     D  Library               11     20A   Inz(*Blanks)
      *
     D LibPath         Ds            20    Qualified
     D  Name                   1     10A   Inz(*Blanks)
     D  Library               11     20A   Inz('QSYS')
      *
     D RcvSpc          Ds
     D  OffSet                 1      4B 0 Inz(*Zeros)
     D  NoEntr                 9     12B 0 Inz(*Zeros)
     D  LstSiz                13     16B 0 Inz(*Zeros)
      *
     D SortList        Ds            16    Qualified
     D  Volume                 1      4I 0 Inz(*Zeros)
     D  Left                   5      8I 0 Inz(*Zeros)
     D  Alloc                  9     12I 0 Inz(*Zeros)
     D  Array                 13     16I 0 Inz(*Zeros)
      *
     D LibraryList     Ds           100
     D  WLib                         10A   Dim(10) Inz(*Blanks)
     D  WLib01                 1     10A
     D  WLib02                11     20A
     D  WLib03                21     30A
     D  WLib04                31     40A
     D  WLib05                41     50A
     D  WLib06                51     60A
     D  WLib07                61     70A
     D  WLib08                71     80A
     D  WLib09                81     90A
     D  WLib10                91    100A
      *
      * Retrieve data queue description
     D/Copy Qsysinc/Qrpglesrc,Qmhqrdqd
      * Retrieve object description
     D/Copy Qsysinc/Qrpglesrc,Qusrobjd
      * Error information
     D/Copy Qsysinc/Qrpglesrc,Qusec
      *
      *---------------------------------------------------------------------------------------------
      * Local Variables
      *---------------------------------------------------------------------------------------------
      *
      * Use with APIs
     D FormatName      S              8    Inz('OBJL0100')
     D ObjectType      S             10A   Inz('*DTAQ')
     D ExtendedAtr     S             10A   Inz('USRSPC')
     D InitSize        S             10I 0 Inz(10000)
     D InitValue       S              1A   Inz(*Blanks)
     D PubAuth         S             10A   Inz('*ALL')
     D Text            S             50A   Inz('Working User Space')
     D StrPos          S             10I 0 Inz(*Zeros)
     D LenDta          S             10I 0 Inz(*Zeros)
     D Receiver        S             30A   Inz(*Blanks)
     D Replace         S             10A   Inz('*YES')
     D ErrorCode       S                   Like(Qusec) Inz(*Blanks)
     D RecvLen         S             10I 0 Inz(%Size(Qmhd0100))
     D DescFormat      S              8A   Inz('RDQD0100')
     D ExtCmd          S            512A   Inz(*Blanks)
     D ExtCmdLen       S             15P 5 Inz(%Size(ExtCmd))
     D DqNam           S             10A   Inz('PVQDEPUS')
     D DqLib           S             10A   Inz('QTEMP')
     D DqLen           S              5P 0 Inz(*Zeros)
     D DqMsg           S            640A   Inz(*Blanks)
     D DqWait          S              5P 0 Inz(*Zeros)
     D AuthInd         S              1A   Inz(*Blanks)
     D UsrPrf          S             10A   Inz('*CURRENT')
     D ObjectTyp       S             10A   Inz(*Blanks)
     D Auth            S             20A   Inz(*Blanks)
     D AuthN           S             10I 0 Inz(*Zeros)
     D Level           S             10I 0 Inz(1)
     D ReturnCode      S             10I 0 Inz(*Zeros)
     D CmdBuffHndl     S             10I 0 Inz(*Zeros)
     D LowEnvmHndl     S             10I 0 Inz(*Zeros)
     D CurRow          S             10I 0 Inz(*Zeros)
     D CurCol          S             10I 0 Inz(*Zeros)
     D LibDetl         S                   Like(Qusd0100)
     D LibDetlLen      S             10I 0 Inz(%Size(LibDetl))
     D FmtName         S              8A   Inz('OBJD0100')
     D LibType         S             10A   Inz('*LIB')
      *
      * Use as memory
     D CFeq            S             10I 0 Inz(5)
     D OFeq            S             10I 0 Inz(5)
     D KFeq            S             10I 0 Inz(5)
     D CWatch          S             10I 0 Inz(50)
     D CCrc            S             10I 0 Inz(100)
     D OCrc            S             10I 0 Inz(100)
     D KCrc            S             10I 0 Inz(100)
     D LstDq           S                   Like(Qmhd0100) Dim(9999) Inz(*Blanks)
     D LstDqVol        S                   Like(SortList) Dim(9999)
     D                                     Inz(*AllX'00') Descend
     D Perc            S              6A   Inz(*Blanks)
     D CLibList        S                   Like(LibraryList) Inz('*LIBL')
     D OLibList        S                   Like(LibraryList) Inz('*LIBL')
     D KLibList        S                   Like(LibraryList) Inz('*LIBL')
     D ListOfLib       S             10A   Dim(10) Inz(*Blanks)
     D ListOfLibN      S             10I 0 Inz(*Zeros)
     D ListOfLibN2     S             10I 0 Inz(*Zeros)
      *
      * Use as indicators
     D xRefresh        S              1N   Inz(*Off)
     D xAlert          S              1N   Inz(*Off)
     D xDftChange      S              1N   Inz(*Off)
     D xReload         S              1N   Inz(*Off)
     D xPageDown       S              1N   Inz(*Off)
     D xPageUp         S              1N   Inz(*Off)
      *
      * Use as counters
     D Percn           S              4S 1 Inz(*Zeros)
     D Arrn            S             10I 0 Inz(*Zeros)
     D LastRecord      S             10I 0 Inz(*Zeros)
     D ElemLoaded      S             10I 0 Inz(*Zeros)
     D G               S             10I 0 Inz(*Zeros)
     D N               S             10I 0 Inz(*Zeros)
     D P               S             10I 0 Inz(*Zeros)
     D R               S             10I 0 Inz(*Zeros)
     D V               S             10I 0 Inz(*Zeros)
     D X               S             10I 0 Inz(*Zeros)
     D Y               S             10I 0 Inz(*Zeros)
      *
      * Use as temporary fields
     D ExQue           S                   Like(DqNam) Inz(*Blanks)
      *
      *---------------------------------------------------------------------------------------------
      * Local Constant
      *---------------------------------------------------------------------------------------------
      *
     D CSflSize        C                   Const(14)
      *
      *---------------------------------------------------------------------------------------------
      * Keys and parameters list
      *---------------------------------------------------------------------------------------------
      *
     C     WQuscrtus     Plist
     C                   Parm                    UserSpace
     C                   Parm                    ExtendedAtr
     C                   Parm                    InitSize
     C                   Parm                    InitValue
     C                   Parm                    PubAuth
     C                   Parm                    Text
     C                   Parm                    Replace
     C                   Parm                    ErrorCode
      *
     C     WQusrtvus     Plist
     C                   Parm                    UserSpace
     C                   Parm                    StrPos
     C                   Parm                    LenDta
     C                   Parm                    Receiver
      *
     C     WQusdltus     Plist
     C                   Parm                    UserSpace
     C                   Parm                    ErrorCode
      *
     C     WQuslobj      Plist
     C                   Parm                    UserSpace
     C                   Parm                    FormatName
     C                   Parm                    ObjectPath
     C                   Parm                    ObjectType
      *
     C     WQhmqrdqd     Plist
     C                   Parm                    Qmhd0100
     C                   Parm                    RecvLen
     C                   Parm                    DescFormat
     C                   Parm                    QueuePath
      *
     C     WQrcvdtaq     Plist
     C                   Parm                    DqNam
     C                   Parm                    DqLib
     C                   Parm      *Zeros        DqLen
     C                   Parm      *Blanks       DqMsg
     C                   Parm      CFeq          DqWait
      *
     C     WQsycusra     Plist
     C                   Parm                    AuthInd
     C                   Parm                    UsrPrf
     C                   Parm                    ObjectDir
     C                   Parm                    ObjectTyp
     C                   Parm                    Auth
     C                   Parm                    AuthN
     C                   Parm                    Level
     C                   Parm                    ErrorCode
      *
     C     WQusrobjd     Plist
     C                   Parm                    LibDetl
     C                   Parm                    LibDetlLen
     C                   Parm                    FmtName
     C                   Parm                    LibPath
     C                   Parm                    LibType
     C                   Parm                    ErrorCode
      *
     C     WQcmdexc      Plist
     C                   Parm                    ExtCmd
     C                   Parm                    ExtCmdLen
      *
      *---------------------------------------------------------------------------------------------
      * Main logic
      *---------------------------------------------------------------------------------------------
      *
     C                   ExSr      SrCrtDtq                                     . Create data queue.
     C                   ExSr      SrCrtOvr                                     . Override display.
     C                   Open      Pvqdepusd                                    . Open display.
      *
     C     #Restart      Tag                                                    . Restart point.
     C                   Reset                   ElemLoaded
     C                   Reset                   LastRecord
     C                   Z-Add     *Zeros        X
     C                   ExSr      SrCrtUsp                                     . Create Userspace.
      *
     C                   If        (WLib(1) = *Blanks)                          . If no library list
     C                   Movel(P)  CLibList      LibraryList                      then default to
     C                   EndIf                                                    library list.
      *
     C                   Z-Add     1             N                              . Retrieve list of
     C                   DoW       (WLib(N) <> *Blanks)                           data queues from
     C                   ExSr      SrLstObj                                       given libraries.
     C                   If        ((N + 1) <= (%Elem(WLib)))
     C                   Add       1             N
     C                   Else
     C                   Leave
     C                   EndIf
     C                   EndDo
     C                   If        (N < (%Elem(WLib))) And
     C                             (N > 1)
     C                   Sub       1             N
     C                   EndIf
      *
     C                   ExSr      SrDltUsp                                     . Delete userspace.
      *
     C                   ExSr      SrRetrInf                                    . Get fresh data.
     C                   Move      *On           xRefresh
      *
     C                   DoW       (*In03 = *Off) And (*In12 = *Off)
      *
     C                   ExSr      SrLoadSfl                                    . Load subfile.
     C                   If        (xAlert = *On)                               . If contain red
     C                   ExSr      SrMakeBeep                                     items, then send
     C                   Move      *Off          xAlert                           a beep.
     C                   EndIf
     C                   Write     Sflc                                 31      . Display screen
      *                                                                           but do not wait.
     C                   If        (*In31 = *On) And                            . On error of
     C                             (QMsgTyp = 'CPF' And QMsgNbr = '4737')         CPF4737, force
     C                   Read      Sflc                                           read on screen.
     C                   Goto      #Select
     C                   EndIf
      *
     C                   Call      'QRCVDTAQ'    WQrcvdtaq                      . Sleep while wait
      *                                                                           for user input.
     C                   If        (DqLen > *Zeros)                             . If input received
     C                   Read      Sflc                                           then read screen,
     C                   Else                                                     else assume screen
     C                   ExSr      SrRetrInf                                      auto-refresh.
     C                   Move      *On           xRefresh
     C                   Goto      #EndLoop
     C                   EndIf
      *
     C     #Select       Tag
      *
     C                   Select
      *
     C                   When      (*In71 = *On)                                . Next record.
     C                   Move      *Off          *In71
     C                   Move      *On           xPageDown
      *
     C                   When      (*In72 = *On)                                . Previous record.
     C                   Move      *Off          *In72
     C                   Move      *On           xPageUp
      *
     C                   EndSl
      *
     C                   Select
      *
     C                   When      (*In03 = *On) Or (*In12 = *On)               . Terminate.
     C                   Iter
      *
     C                   When      (*In05 = *On)                                . Refresh.
     C                   ExSr      SrRetrInf
     C                   Move      *On           xRefresh
     C                   Iter
      *
     C                   When      (*In07 = *On)                                . Change parameters.
     C                   ExSr      SrChgDft
     C                   If        (*In03 = *On)
     C                   Iter
     C                   EndIf
     C                   Move      *On           xDftChange
     C                   If        (xReload = *On)                              . Library reload
     C                   Move      *Off          xReload                          required. Goto
     C                   Goto      #Restart                                       restart point.
     C                   EndIf
      *
     C                   When      (*In09 = *On)                                . Show information.
     C                   ExSr      SrDspInf
     C                   If        (*In03 = *On)
     C                   Iter
     C                   EndIf
      *
     C                   EndSl
      *
     C     #EndLoop      Tag
     C                   EndDo
      *
     C     #EndPgm       Tag
     C                   Close     Pvqdepusd                                    . Close display.
     C                   ExSr      SrDltOvr                                     . Delete override.
     C                   ExSr      SrDltDtq                                     . Delete data queue.
     C                   Move      *On           *Inlr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrLoadSfl
      *  Loads subfile display
      *---------------------------------------------------------------------------------------------
      *
     C     SrLoadSfl     BegSr
      *
     C                   Z-Add     *Zeros        Arrn
     C                   Move      *On           *In55
     C   81              Move      *Off          *In81
      *
     C                   Write     SflC
     C                   If        (*In31 = *On) And                            . On error of
     C                             (QMsgTyp = 'CPF' And QMsgNbr = '4737')         CPF4737, force
     C                   Read      Sflc                                           read on screen.
     C                   EndIf
      *
     C                   Select
      *
     C                   When      (xPageDown = *On)
     C                   If        (LastRecord < P)
     C     LastRecord    Add       1             G
     C                   Else
     C                   Move      *On           *In81
     C                   EndIf
     C                   Move      *Off          xPageDown
      *
     C                   When      (xPageUp = *On)
     C                   If        ((LastRecord - CSflSize) > *Zeros)
     C                   If        (LastRecord > CSflSize)
     C     LastRecord    Sub       ElemLoaded    LastRecord
     C                   EndIf
     C     LastRecord    Sub       CSflSize      G
     C                   Add       1             G
     C                   Else
     C                   Move      *On           *In81
     C                   EndIf
     C                   Move      *Off          xPageUp
      *
     C                   Other
     C                   If        (LastRecord <= *Zeros) Or
     C                             (LastRecord > P)
     C                   Z-Add     1             G
     C                   Else
     C     LastRecord    Sub       ElemLoaded    G
     C                   Add       1             G
     C                   EndIf
      *
     C                   EndSl
      *
     C     G             Do        P             R
      *
     C                   Movel(P)  LstDqVol(R)   SortList
     C                   Eval      Qmhd0100 = (LstDq((%Elem(LstDq) -
     C                             (SortList.Array)) + 1))
      *
     C                   Eval      WDqNam = (Qmhdqn)                            . Name.
     C                   Eval      WDqLib = (Qmhdqlib)                          . Library.
     C                   Eval      Percn = ((Qmhnbrm / Qmhmnea) * 100)          . Data queue
     C                   Eval      Perc = (%Char(Percn) + '%')                    utilisation
     C                   EvalR     WPerc = (%Trim(Perc))                          percentage.
     C                   Eval      WMsgAvl = (Qmhnbrm)                          . Available.
     C                   Eval      WMsgInt = (Qmhinbre)                         . Initial.
     C                   Eval      WMsgAlc = (Qmhmnbrm)                         . Allocated.
     C                   Eval      WMsgMax = (Qmhmnea)                          . Maximum.
      *
     C                   Select
      *
     C                   When      ((Qmhmnea - Qmhnbrm) < 150)                  . Almost max.
     C                   Movel(P)  'ALERT'       WRmk
     C                   Move      *On           *In60
      *
     C                   When      (Qmhnbrm >= CCrc)                            . Reached critical.
     C                   Movel(P)  'WARN'        WRmk
     C                   Move      *On           *In60
      *
     C                   When      (Qmhnbrm >= CWatch)                          . Growing fast.
     C                   Movel(P)  'CHECK'       WRmk
      *
     C                   Other                                                  . Normal status.
     C                   Movel(P)  'OK'          WRmk
      *
     C                   EndSl
      *
     C                   Add       1             Arrn                           . Write information
     C                   Write     Sfl                                            to display.
      *
     C                   If        (*In60 = *On) And
     C                             (xAlert = *Off)
     C                   Move      *On           xAlert
     C                   EndIf
      *
     C   60              Move      *Off          *In60
      *
     C                   If        ((Arrn + 1) > CSflSize)                      . Validate process
     C                   Leave                                                    against subfile
     C                   EndIf                                                    page size.
      *
     C                   EndDo
      *
     C                   If        (R > P)
     C                   Z-Add     P             R
     C                   EndIf
      *
     C                   Z-Add     R             LastRecord
     C                   Z-Add     Arrn          ElemLoaded
      *
     C                   If        (R < P)
     C                   Movel(P)  'More...'     WSflInd
     C                   Else
     C                   Movel(P)  ' Bottom'     WSflInd
     C                   EndIf
      *
     C                   If        (Arrn = *Zeros)
     C                   Z-Add     *Zeros        WMsgAvl
     C                   Z-Add     *Zeros        WMsgInt
     C                   Z-Add     *Zeros        WMsgAlc
     C                   Z-Add     *Zeros        WMsgMax
     C                   Move      *On           *In57
     C                   Z-Add     1             Arrn
     C                   Write     Sfl
     C                   Move      *Off          *In57
     C                   Z-Add     1             ElemLoaded
     C                   EndIf
      *
     C                   Move      *Off          *In55
     C                   Z-Add     1             Warrn                          . Set page.
      *
     C                   If        (xRefresh = *On) Or
     C                             (xDftChange = *On)
     C                   Write     D01                                          . Write header
     C                   EndIf
     C                   Write     D02                                          . Write footer
      *
     C                   If        (xRefresh = *On)
     C                   Move      *Off          xRefresh
     C                   EndIf
      *
     C                   If        (xDftChange = *On)
     C                   Move      *Off          xDftChange
     C                   EndIf
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrRetrInf
      *  Retrieve information on data queue
      *---------------------------------------------------------------------------------------------
      *
     C     SrRetrInf     BegSr
      *
     C                   Z-Add     *Zeros        P
     C                   Reset                   LstDq(*)
     C                   Reset                   LstDqVol(*)
      *
     C                   Do        X             Y
      *
     C     Y             Occur     ObjectInfo
      *
     C                   Reset                   Qmhd0100
     C                   Eval      QueuePath.Name = (ObjectInfo.Name)
     C                   Eval      QueuePath.Library = (ObjectInfo.Library)
      *
     C                   Monitor
     C                   Call      'QMHQRDQD'    WQhmqrdqd                      . Get information
      *                                                                           from data queue.
     C                   Add       1             P                              . Store information
     C                   Eval      LstDq(P) = (Qmhd0100)                          in memory.
     C                   Eval      SortList.Volume = (Qmhnbrm)
     C                   Eval      SortList.Left = ((Qmhnbrm / Qmhmnea) * 100)
     C                   Eval      SortList.Alloc = (Qmhmnbrm)
     C                   Eval      SortList.Array = (((%Elem(LstDq) - P) + 1))
     C                   Eval      LstDqVol(P) = (SortList)
      *
     C                   On-Error
     C                   EndMon
      *
     C                   EndDo
      *
     C                   SortA     LstDqVol                                     . Sort according to
      *                                                                           volume.
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrMakeBeep
      *  Generates a beep message
      *---------------------------------------------------------------------------------------------
      *
     C     SrMakeBeep    BegSr
      *
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
      *
     C                   Eval      ReturnCode = MakeBeep ( CmdBuffHndl :
     C                                                     LowEnvmHndl :
     C                                                     ErrorCode     )
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrLstObj
      *  List objects in library
      *---------------------------------------------------------------------------------------------
      *
     C     SrLstObj      BegSr
      *
     C                   Eval      ObjectPath.Library = (WLib(N))
     C                   Call      'QUSLOBJ'     WQuslobj
      *
     C                   Z-Add     125           StrPos
     C                   Z-Add     16            LenDta
     C                   ExSr      SrRtvUsp
     C                   Movel(P)  Receiver      RcvSpc
     C     OffSet        Add       1             StrPos
     C                   Z-Add     LstSiz        LenDta
      *
     C                   Do        NoEntr        V
     C                   Add       1             X
     C     X             Occur     ObjectInfo
     C                   ExSr      SrRtvUsp
     C                   Movel(P)  Receiver      ObjectInfo
      *
     C                   If        (ObjectInfo.Library = 'QTEMP') And
     C                             (ObjectInfo.Name = 'PVQDEPUS')
     C                   Clear                   ObjectInfo
     C                   Sub       1             X
      *
     C                   Else
     C                   Eval      ExQue = ObjectInfo.Name
     C     ExQue         Chain     RPvqdepus
     C                   If        (%Found) And
     C                             ((ObjectInfo.Name = DtqNam And
     C                               ObjectInfo.Library = DtqLib) Or
     C                              (ObjectInfo.Name = DtqNam And
     C                               ObjectInfo.Library = '*LIBL') Or
     C                              (ObjectInfo.Name = DtqNam And
     C                               ObjectInfo.Library = *Blanks))
     C                   Clear                   ObjectInfo
     C                   Sub       1             X
      *
     C                   Else
     C                   ExSr      SrChkAuth
     C                   If        (AuthInd = 'N')
     C                   Clear                   ObjectInfo
     C                   Sub       1             X
     C                   EndIf
      *
     C                   EndIf
     C                   EndIf
      *
     C     StrPos        Add       LenDta        StrPos
     C                   EndDo
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrChkAuth
      *  Check authority
      *---------------------------------------------------------------------------------------------
      *
     C     SrChkAuth     BegSr
      *
     C                   Eval      ObjectDir.Name = (ObjectInfo.Name)
     C                   Eval      ObjectDir.Library = (ObjectInfo.Library)
     C                   Eval      ObjectTyp = ('*DTAQ')
     C                   Eval      Auth = ('*OBJOPR   *READ     ')
     C                   Eval      AuthN = (2)
      *
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
      *
     C                   Call      'QSYCUSRA'    WQsycusra
      *
     C                   If        (AuthInd = 'Y')
     C                   Eval      ObjectDir.Name = (ObjectInfo.Library)
     C                   Eval      ObjectDir.Library = ('QSYS')
     C                   Eval      ObjectTyp = ('*LIB')
     C                   Eval      Auth = ('*EXECUTE  ')
     C                   Eval      AuthN = (1)
      *
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
      *
     C                   Call      'QSYCUSRA'    WQsycusra
     C                   EndIf
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrChgDft
      *  Change default settings
      *---------------------------------------------------------------------------------------------
      *
     C     SrChgDft      BegSr
      *
     C                   If        (WFeq = *Zeros)
     C                   Z-Add     CFeq          WFeq
     C                   EndIf
      *
     C                   If        (WCrc = *Zeros)
     C                   Z-Add     CCrc          WCrc
     C                   EndIf
      *
     C                   Z-Add     WFeq          KFeq
     C                   Z-Add     WCrc          KCrc
     C                   Movel(P)  LibraryList   KLibList
      *
     C     #ReDisplay    Tag
     C                   Write     D01
     C                   Exfmt     D03
      *
     C                   MoveA     '0000000000'  *In(41)
     C                   Move      *Blanks       WErrDesc
      *
     C                   Select
      *
     C                   When      (*In03 = *On)
     C                   LeaveSr
      *
     C                   When      (*In08 = *On)
     C                   Z-Add     OFeq          WFeq
     C                   Z-Add     OCrc          WCrc
     C                   Movel(P)  OLibList      LibraryList
     C                   Goto      #ReDisplay
      *
     C                   When      (*In12 = *On)
     C                   Z-Add     KFeq          WFeq
     C                   Z-Add     KCrc          WCrc
     C                   Movel(P)  KLibList      LibraryList
     C                   Move      *Off          *In12
     C                   LeaveSr
      *
     C                   EndSl
      *
     C                   If        (%Subst(LibraryList:1:100) = *Blanks)
     C                   Movel(P)  OLibList      LibraryList
     C                   Goto      #ReDisplay
     C                   EndIf
      *
     C                   If        (WLib01 = '*LIBL') And
     C                             (%Subst(LibraryList:11:90) <> *Blanks)
     C                   Move      *On           *In41
     C                   Eval      WErrDesc = ('Cannot use *LIBL when other ' +
     C                             'libraries are also specified.')
     C                   Goto      #ReDisplay
     C                   EndIf
      *
     C                   If        (%Subst(WLib02:1:1) = '*') Or
     C                             (%Subst(WLib03:1:1) = '*') Or
     C                             (%Subst(WLib04:1:1) = '*') Or
     C                             (%Subst(WLib05:1:1) = '*') Or
     C                             (%Subst(WLib06:1:1) = '*') Or
     C                             (%Subst(WLib07:1:1) = '*') Or
     C                             (%Subst(WLib08:1:1) = '*') Or
     C                             (%Subst(WLib09:1:1) = '*') Or
     C                             (%Subst(WLib10:1:1) = '*')
     C                   Move      *On           *In41
     C                   Eval      WErrDesc = ('Special or reserved keywords ' +
     C                             '* only for value 1.')
     C                   Goto      #ReDisplay
     C                   EndIf
      *
     C                   If        (%Subst(WLib01:1:1) = '*') And
     C                             ((WLib01 <> '*LIBL') And
     C                              (WLib01 <> '*CURLIB'))
     C                   Move      *On           *In41
     C                   Eval      WErrDesc = ('Specify either library name, ' +
     C                             '*LIBL, or *CURLIB only.')
     C                   Goto      #ReDisplay
     C                   EndIf
      *
     C                   If        (%Subst(WLib01:1:1) <> '*') And
     C                             (WLib01 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib01)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In41
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib02 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib02)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In42
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib03 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib03)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In43
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib04 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib04)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In44
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib05 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib05)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In45
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib06 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib06)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In46
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib07 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib07)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In47
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib08 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib08)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In48
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib09 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib09)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In49
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   If        (WLib10 <> *Blanks)
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
     C                   Eval      LibPath.Name = (WLib10)
     C                   Call      'QUSROBJD'    WQusrobjd
     C                   Movel(P)  ErrorCode     Qusec
     C                   If        (Qusei <> *Blanks)
     C                   Move      *On           *In50
     C                   Eval      WErrDesc = ('Error ' + (%TrimR(Qusei)) +
     C                             ' when searching library. Check job log.')
     C                   Goto      #ReDisplay
     C                   EndIf
     C                   EndIf
      *
     C                   Z-Add     *Zeros        ListOfLibN
     C                   Z-Add     *Zeros        ListOfLibN2
     C                   Clear                   ListOfLib(*)
     C                   For       ListOfLibN2 = 1 By 1 To (%Elem(WLib))
     C                   If        (WLib(ListOfLibN2) <> *Blanks)
     C                   Add       1             ListOfLibN
     C                   Eval      ListOfLib(ListOfLibN) = (WLib(ListOfLibN2))
     C                   EndIf
     C                   EndFor
     C                   Movea(P)  ListOfLib     LibraryList
      *
     C                   Z-Add     WFeq          CFeq
     C                   Z-Add     WCrc          CCrc
     C                   Eval      CWatch = (CCrc / 2)
     C                   If        (LibraryList <> CLibList)
     C                   Movel(P)  LibraryList   CLibList
     C                   Move      *On           xReload
     C                   EndIf
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrDspInf
      *  Display more information
      *---------------------------------------------------------------------------------------------
      *
     C     SrDspInf      BegSr
      *
     C                   Clear                   D04
      *
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
      *
     C                   Eval      ReturnCode = GetCurAddr ( CurRow :
     C                                                       CurCol :
     C                                                       LowEnvmHndl :
     C                                                       ErrorCode     )
      *
     C                   If        (ReturnCode < 0) Or
     C                             (CurRow = 0) Or (CurCol = 0) Or
     C                             (CurRow < 7) Or (CurRow > 20)
     C                   Eval      iErrDesc = 'Cursor address capture failed.'
      *
     C                   Else
     C                   Eval      CurRow -= 6
     C     CurRow        Chain     Sfl
     C                   If        Not(%Found) Or
     C                             (wDqLib = *Blanks) Or (wDqNam = *Blanks)
     C                   Eval      iErrDesc = 'Invalid subfile record.'
      *
     C                   Else
     C                   Eval      iQueuePath = '/QSYS.LIB/' +
     C                                          %Trim(wDqLib) + '.LIB/' +
     C                                          %Trim(wDqNam) + '.DTAQ'
      *
     C                   Reset                   Qmhd0100
     C                   Eval      QueuePath.Name = (wDqNam)
     C                   Eval      QueuePath.Library = (wDqLib)
      *
     C                   Monitor
     C                   Call      'QMHQRDQD'    WQhmqrdqd
      *
     C                   Eval      iMsgLen = Qmhml
     C                   Eval      iKeyLen = Qmhkl
      *
     C                   If        (Qmhuence = 'F')
     C                   Eval      iSeq = '*FIFO'
     C                   ElseIf    (Qmhuence = 'K')
     C                   Eval      iSeq = '*KEYED'
     C                   ElseIf    (Qmhuence = 'L')
     C                   Eval      iSeq = '*LIFO'
     C                   Else
     C                   Eval      iSeq = '*UNKNOWN'
     C                   EndIf
      *
     C                   If        (Qmhisi = 'Y')
     C                   Eval      iSenderId = '*YES'
     C                   ElseIf    (Qmhisi = 'N')
     C                   Eval      iSenderId = '*NO'
     C                   Else
     C                   Eval      iSenderId = '*UNKNOWN'
     C                   EndIf
      *
     C                   If        (Qmhfi = 'Y')
     C                   Eval      iForce = '*YES'
     C                   ElseIf    (Qmhfi = 'N')
     C                   Eval      iForce = '*NO'
     C                   Else
     C                   Eval      iForce = '*UNKNOWN'
     C                   EndIf
      *
     C                   If        (Qmhtype01 = '0')
     C                   Eval      iType = '*STD'
     C                   ElseIf    (Qmhtype01 = '1')
     C                   Eval      iType = '*DDM'
     C                   Else
     C                   Eval      iType = '*UNKNOWN'
     C                   EndIf
      *
     C                   If        (Qmhar = '0')
     C                   Eval      iReclaim = '*NO'
     C                   ElseIf    (Qmhar = '1')
     C                   Eval      iReclaim = '*YES'
     C                   Else
     C                   Eval      iReclaim = '*UNKNOWN'
     C                   EndIf
      *
     C                   Eval      iAvail = Qmhnbrm
     C                   Eval      iInitial = Qmhinbre
     C                   Eval      iAllocate = Qmhmnbrm
     C                   Eval      iMaximum = Qmhmnea
      *
     C                   On-Error
     C                   Eval      iErrDesc = 'Queue probe failed.'
     C                   EndMon
      *
     C                   EndIf
     C                   EndIf
      *
     C                   Write     D01
     C                   Exfmt     D04
      *
     C                   Select
      *
     C                   When      (*In03 = *On)
     C                   LeaveSr
      *
     C                   When      (*In12 = *On)
     C                   Move      *Off          *In12
     C                   LeaveSr
      *
     C                   EndSl
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrCrtUsp
      *  Create user space
      *---------------------------------------------------------------------------------------------
      *
     C     SrCrtUsp      BegSr
      *
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
      *
     C                   Call      'QUSCRTUS'    WQuscrtus
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrRtvUsp
      *  Retrieve user space
      *---------------------------------------------------------------------------------------------
      *
     C     SrRtvUsp      BegSr
      *
     C                   Call      'QUSRTVUS'    WQusrtvus
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrDltUsp
      *  Delete user space
      *---------------------------------------------------------------------------------------------
      *
     C     SrDltUsp      BegSr
      *
     C                   Reset                   Qusec
     C                   Eval      Qusbprv = (%Size(Qusec))
     C                   Z-Add     *Zeros        Qusbavl
     C                   Movel(P)  Qusec         ErrorCode
      *
     C                   Call      'QUSDLTUS'    WQusdltus
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrCrtDtq
      *  Create data queue
      *---------------------------------------------------------------------------------------------
      *
     C     SrCrtDtq      BegSr
      *
     C                   Eval      ExtCmd = ('CRTDTAQ DTAQ(QTEMP/PVQDEPUS) ' +
     C                             'MAXLEN(640) SENDERID(*YES) AUTORCL(*YES) ' +
     C                             'AUT(*ALL)')
      *
     C                   Call      'QCMDEXC'     WQcmdexc               3030
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrDltDtq
      *  Delete data queue
      *---------------------------------------------------------------------------------------------
      *
     C     SrDltDtq      BegSr
      *
     C                   Eval      ExtCmd = ('DLTDTAQ DTAQ(QTEMP/PVQDEPUS)')
      *
     C                   Call      'QCMDEXC'     WQcmdexc               3030
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrCrtOvr
      *  Override display file
      *---------------------------------------------------------------------------------------------
      *
     C     SrCrtOvr      BegSr
      *
     C                   Eval      ExtCmd = ('OVRDSPF FILE(PVQDEPUSD) ' +
     C                             'DTAQ(QTEMP/PVQDEPUS) SHARE(*YES) ' +
     C                             'SECURE(*YES)')
      *
     C                   Call      'QCMDEXC'     WQcmdexc               3030
      *
     C                   EndSr
      *
      *---------------------------------------------------------------------------------------------
      * Sub Routine SrDltOvr
      *  Delete override over display file
      *---------------------------------------------------------------------------------------------
      *
     C     SrDltOvr      BegSr
      *
     C                   Eval      ExtCmd = ('DLTOVR FILE(*ALL)')
      *
     C                   Call      'QCMDEXC'     WQcmdexc               3030
      *
     C                   EndSr
      *
