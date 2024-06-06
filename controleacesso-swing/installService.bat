set SERVICE_NAME=SmartAcesso
set PR_INSTALL=C:\ambiente\prunsrv.exe

REM Service log configuration
set PR_LOGPREFIX=%SERVICE_NAME%
set PR_LOGPATH=c:\logs
set PR_STDOUTPUT=c:\logs\stdout.txt
set PR_STDERROR=c:\logs\stderr.txt
set PR_LOGLEVEL=Error

REM Path to java installation
set PR_CLASSPATH=SmartAcessoDesktop.jar

REM Startup configuration
set PR_STARTUP=auto
set PR_STARTMODE=java
set PR_STARTCLASS=com.protreino.services.main.Main
set PR_STARTMETHOD=main

REM JVM configuration
set PR_JVMMS=256
set PR_JVMMX=1024
set PR_JVMSS=4000

REM Install service
prunsrv.exe //IS//%SERVICE_NAME%