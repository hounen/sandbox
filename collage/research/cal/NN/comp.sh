#!/bin/bash
g77 -c main.f eigenH.f diago.f func.f matele.f
g77 -o 5Li main.o eigenH.o diago.o func.o matele.o

