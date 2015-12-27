A Distributed File System

List of implemented modules:

*Distributed Transparent File Access - This is the core of any distributed file system and consists of a TCP server which provides
access to files on the machine on which it is executed and a client side file service proxy that provides a language specific interface 
to the file system. 

*Directory Service - The directory service is responsible for mapping human readable, global file names into file identifiers used by 
the file system itself.A user request to open a particular file X is passed by the client proxy to the directory server for
resolution.

*Caching - Caching is a vital element of any file system design that is required to give good performance and scale.

*Lock Service -  This server simply holds a semaphore for each file it is told about. Any client wishing to access a file could simply 
ask for access from the lock server. 
