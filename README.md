# CS4032-Project

NOTE: This does not run, unfinished

API pulled from https://github.com/simonq80/CS4032-API

## Setup
In each services directory run:
* stack build
* docker-compose up

## TODO:
- [x] Client Proxy
- [x] Security Server
- [x] Directory Server
- [x] File Server
- [ ] Encryption
- [ ] Replication
- [ ] Transactions
- [ ] Caching 
- [ ] Locking
- [x] Example Client Application

## Client Proxy
Client Proxy handles communication to all other servers to read/write a file.
* Goes to security server to get auth token
* Goes to directory server with auth token and file details
* Reads from/Writes to File Server with file details returned by directory server
(Login details can by set by client application

## Security Server
Handles user authentication and token generation
* Returns token on correct login
* Passes token on to directory server

## Directory Server
Maps file info to file location
* Takes file info & token
* Returns server & file info (Generates new location if does not exist)
* Passes token to file server specified

## File Server
Contains actual files
* Read:
  * Takes file info
  * Returns file info (empty file contents if does not exist yet)
* Write
  * Takes file info
  * Writes to DB
  * Returns true on success


