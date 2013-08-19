      *------------------------------------------------------------------------
      *  displays a view over files in a IFS directory, sorted on name. 
      *  uses a load all subfile
      *  with option 1 a pointer to the filename is returned. 
      *
      *  Compile and link (only runs on OS/400 under 5250): 
      *  crtdspf on the display file, then crtrpgmod and crtsrvpgm on this. nothing fancy.
      *    
      *------------------------------------------------------------------------

     h nomain option(*nodebugio)
     h bnddir('QC2LE')
     fdirviewws cf   e             workstn sfile(sfl1: rrn1) usropn
     f                                     indds(indicators)
     f                                     infds(info)

     d info            ds
     d  cfkey                369    369
     d  enter          c                   const(x'F1')

     d indicators      ds
     d  f3                    03     03n
     d  sflclr                30     30n
     d  sfldsp                35     35n
     d  sflend                90     90n   inz('0')
     d  error                 20     20n
     d  displayMsg            60     60n

     d rrn1            s             10i 0
      
     d sltIFSfile      pr              *
     d  ifsdir                       40    varying

     d compare         pr            10i 0
     d a                                   like(longfname)
     d b                                   like(longfname)

     d memcmp          pr            10i 0 extproc('memcmp')
     d                                 *   value
     d                                 *   value
     d                               10u 0 value

     d opendir         pr              *   extproc('opendir')
     d  dirname                        *   value options(*string)

     d rewinddir       pr                  extproc('rewinddir')
     d  dirname                        *   value

     d readdir         pr              *   extproc('readdir')
     d  dirp                           *   value

     d closedir        pr            10i 0 extproc('closedir')
     d  dirhandle                      *   value

     d stat            pr            10i 0 extproc('stat64')
     d   path                          *   value options(*string)
     d   buf                           *   value

     d callsystem      pr            10i 0 extproc('system')
     d  cmdstring                      *   value options(*string)

     d perror          pr                  extproc('perror')
     d  text                           *   value options(*string)

     d printf          pr            10i 0 extproc('printf')
     d                                 *   value options(*string)

     d getchar         pr            10i 0 extproc('getchar')

     d qsort           pr                  extproc('qsort')
     d                                 *   value
     d                               10u 0 value
     d                               10u 0 value
     d                                 *   procptr value

      
     p sltIFSfile      b                   export
     d sltIFSfile      pi              *
     d  ifsdir                       40    varying
     d rc              s             10i 0
     d path            s             10
     d pathx           s             10
     d pathp           s               *   inz
     d diropn          s             10i 0
     d diropnp         s               *   inz
      
     dentryp           s               *
     dentry            ds                  based(entryp)
     d reserv1                       16
     d filenogenid                   10u 0
     d fileno                        10u 0
     d reclen                        10u 0
     d reserv3                       10i 0
     d reserv4                        8
     d ccsid                         10i 0
     d countryid                      2
     d langid                         3
     d nlsreserv                      3
     d namelen                       10u 0
     d namedir                      640

     d p_statds        s               *
     d statds          ds                  based(p_statds)
     d  st_mode                      10u 0
     d  st_ino                       10u 0
     d  st_uid                       10u 0
     d  st_gid                       10u 0
     d  st_size                      20i 0
     d  st_atime                     10i 0
     d  st_mtime                     10i 0
     d  st_ctime                     10i 0
     d  st_dev                       10u 0
     d  st_blksize                   10u 0
     d  st_nlink                      5u 0
     d  st_codepage                   5u 0
     d  st_allocsize                 20u 0
     d  st_ino_gen_id                10u 0
     d  st_objtype                   12a
     d  st_reserved                   5a
     d  st_rdev                      10u 0
     d  st_rdev64                    20u 0
     d  st_dev64                     20u 0
     d  st_nlink32                   10u 0
     d  st_vfs                       10u 0
     d  reserved                     22a
     d  st_ccsid                      5u 0

     d mystat          s                   like(statds)
     d myobjtype       s             12a

     d command         s            512
     d name            s             50
     d namefile        s             50
     d sflsiz          s             10i 0
     d index           s             10i 0
     d temprrn         s             10i 0
     d pos             s             10i 0
     d i               s             10i 0 inz(1)
     d sltfile         s            512    varying
     d psltfile        s               *
     d nbrFiles        s             10i 0 inz(0)
     d fileArray       s                   like(longfname) dim(10000)
     d                                     based(pFileArray)
     d pFileArray      s               *   inz(*null)
     d wsFileNameSize  s             10i 0 inz(%len(wsifsfile))

      /free
       diropnp = opendir(%trimr(ifsdir));

       if diropnp = *NULL;
         perror('opendir failure');
         printf(('Press ENTER to continue' + x'25'));
         getchar();
         return *NULL;
       endif;

       wsifsdir = ifsdir;

       exsr countFilesInDir;
       rewinddir(diropnp);       
       pFileArray = %alloc(%size(longfname) * nbrFiles);

       open dirviewws;
       exsr clrSfil;
       exsr loadSfil;

       dow not f3;
         write fkeys;
         exfmt sfl1ctl;

         exsr clearMsg;
         select;
         when f3;
           leave;
         when cfkey = enter;
           exsr processOpts;
           if psltfile <> *null;
             leave;
           endif;
         endsl;
       enddo;

       closedir(diropnp);

       close dirviewws;
       f3 = *off;
       dealloc(n) pFileArray;
       return psltfile;

       
       begsr clrSfil;
         sflclr=*on;
         write sfl1ctl;
         sflclr=*off;
         rrn1=0;
         posrrn=1;
       endsr;

       
       begsr loadSfil;

       entryp = readdir(diropnp);
       dow entryp <> *null;
         longfname = %subst(namedir: 1: namelen);
         if longfname <> '.' and longfname  <> '..';
           rc = stat(%trimr(ifsdir) + '/' + %trimr(longfname): %addr(mystat));
           if rc < 0;
             perror('stat64() failure');
             printf(('Press ENTER to continue' + x'25'));
             getchar();
             return *NULL;
           endif;

           p_statds = %addr(mystat);
           myobjtype = st_objtype;
           if %subst(myobjtype: 1: 5) = '*STMF';
             fileArray(i) = longfname;
             i += 1;
           endif;
         endif;
         entryp = readdir(diropnp);
       enddo;

       // RPG IV demands uppercase for the callback func name, linker screams otherwise
       qsort(pFileArray: nbrFiles: %size(FileArray): %paddr('COMPARE') );       
       wsobjtype = myobjtype;
       for i = 1 to nbrFiles;
         if %len(%trimr(fileArray(i))) > wsFileNameSize;
           wsIFSfile = %subst(fileArray(i): 1: wsFileNameSize);
         else;
           wsIFSfile = fileArray(i);
         endif;

         option = ' ';
         rrn1 += 1;
         sflsiz += 1;
         write sfl1;
       endfor;

       if rrn1 <> 0;         
         sfldsp = *on;
       endif;

       endsr;


       begsr processOpts;
           temprrn = rrn1;
           for index = 1 to sflsiz;
             chain index sfl1;
             if not %found;
               leave;
             endif;
             select;
             when option = '1';               
               sltfile = %trimr(ifsdir) + '/' + %trimr(filearray(index));
               psltfile = %addr(sltfile);
               leavesr;
             when option = '5';
               command = 'DSPF STMF(''' + %trimr(ifsdir) + '/' +
                 %trimr(filearray(index)) + ''')';
               if callSystem(command) <> 0;
                 error = *on;
                 displayMsg = *on;
                 msg = 'Error on DSPF';
               endif;
               exsr clearOpt;
             endsl;
           endfor;
           rrn1 = temprrn;
       endsr;


       begsr clearMsg;
         error = *off;
         displayMsg=*off;
         msg = *blank;
       endsr;


       begsr clearOpt;
         posrrn=rrn1;
         option=' ';
         update sfl1;
       endsr;


       begsr countFilesIndir;
         entryp = readdir(diropnp);
         dow entryp <> *null;
           longfname = %subst(namedir: 1: namelen);
           if longfname <> '.' and longfname  <> '..';
             rc = stat(%trimr(ifsdir) + '/' + %trimr(longfname): %addr(mystat));
             if rc < 0;
               perror('stat64() failure');
               printf(('Press ENTER to continue' + x'25'));
               getchar();
               return *NULL;
             endif;
             p_statds = %addr(mystat);
             if %subst(st_objtype: 1: 5) = '*STMF';
               nbrFiles  += 1;
             endif;
           endif;
           entryp = readdir(diropnp);
         enddo;
       endsr;
      /end-free
     p                 e

      
     p compare         b
     d compare         pi            10i 0
     d  elem1                              like(longfname)
     d  elem2                              like(longfname)
      /free
        return memcmp(%addr(elem1): %addr(elem2): %size(longfname));
      /end-free
     p                 e
