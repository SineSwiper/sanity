#!/usr/bin/perl
use Test::More tests => 14;

BEGIN {
   use_ok('insanity');
   use_ok('insanity', $_) for (qw(strict warnings feature ex::caution NO:crap latest sane common::sense insanity));
   use_ok('insanity', '-namespace::clean');
   use_ok('insanity', 'Modern::Perl', '-IO::Handle');
   
   isnt(${^WARNING_BITS}, 0, '^WARNING_BITS check');
   isnt($^H,              0, '^H check');
}
