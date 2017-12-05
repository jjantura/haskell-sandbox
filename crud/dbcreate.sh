#!/bin/bash
sqlite3 crud.db create table crud(id integer primary key, str text); insert into crud(str) values('sample value');
