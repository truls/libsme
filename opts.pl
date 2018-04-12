#!/usr/bin/perl

use warnings;
use strict;
use IPC::Open2;

my %graph = ();

# Slightly modified version of script found on
# https://ro-che.info/articles/2017-07-26-haskell-library-in-c-project

# incdirs, ldopts are collected as hash keys to avoid duplicates
# and because their order does not matter — unlike the order of the libraries
my %incdirs = ();
my %ldopts = ();
my @libfiles;
my @libfiles_static;
my $ghcver = `ghc --numeric-version`;
chomp $ghcver;

sub append_ghc {
    my ($pkgname) = @_;
    return $pkgname. "-ghc$ghcver";
}

sub add_to_graph($);
sub add_to_graph($) {
    my ($pkgname) = @_;

    return if exists $graph{$pkgname};

    open my $ghcpkg, "-|", "ghc-pkg", "field", "--simple-output",
        $pkgname, "depends,hs-libraries,extra-libraries,ld-options,library-dirs,dynamic-library-dirs,include-dirs";

    my %pkg;
    $pkg{'depends'} = [split ' ', <$ghcpkg>];
    my @hslibs_in = split ' ', <$ghcpkg>;
    # my @hslibs = map {
    #     #print $_ . "\n";
    #     /^C/
    #       ? s/^C//r
    #       : ((($_ eq "HSrts") ? "HSrts_thr" : $_) . "-ghc$ghcver")
    # } @hslibs_in;
    my @hslibs = map {
        my $res;
        if ($_ eq "HSrts") {
            $res = "HSrts_thr";
        } else {
            $res = $_;
        };
        $res
    } @hslibs_in;
    # print join("\n", @hslibs) . "\n";
    # my @hslibs = map {
    #     /^C/
    #         ? s/^C//r
    #         : ((($_ eq "HSrts") ? "HSrts_thr" : $_))
    # } split ' ', <$ghcpkg>;
    my @extralibs = split ' ', <$ghcpkg>;
    my @libs = (@hslibs, @extralibs);
    $pkg{'libs'} = \@libs;

    $ldopts{$_}  = 1 for (map { s/^"(.*)"/$1/r; } split(' ', <$ghcpkg>));

    my @libdirs_static = split ' ', <$ghcpkg>;
    my @libdirs_dynamic = (split ' ', <$ghcpkg>);
    # the rts package info does not define dynamic-library-dirs
    unless (@libdirs_dynamic) { @libdirs_dynamic = @libdirs_static };
    for my $lib_base (@hslibs) {
        my $found = 0;
        my $found_static = 0;
        my $lib = append_ghc($lib_base);
        if ( $lib_base =~ /^C(.*)/ ) {
            #print "Name starts with C " . $lib_base . "\n";
            #$_ = $1;
            $lib = s/^C//r for $1;
        } else {
            $lib = append_ghc($lib_base);
        }
        #print "Transformed " . $lib_base . " to " . $lib . "\n";
        # my $lib = {/^C/
        #                ? s/^C//r
        #                : append_ghc($_)
        # };
        #my $lib = $lib_base;
        my $libfile = "lib$lib.so";
        my $libfile_static = "lib$lib_base.a";
        for my $libdir (@libdirs_dynamic) {
            my $path = "$libdir/$libfile";
            if (-e $path) {
                # print $path . "\n";
                push @libfiles, $path;
                $found = 1;
                last;
            }
        }
        for my $libdir (@libdirs_static) {
            #my $libname = s/^HS// for $
            my $path = "$libdir/${libfile_static}";
            #print $path . "\n";
            if (-e $path) {
                push @libfiles_static, $path;
                $found_static = 1;
                last;
            }
        }
        if (!$found) {
            local $,=", ";
            die "Library not found: $libfile\nDirectories searched: @libdirs_dynamic\n";
        }
        if (!$found_static) {
            local $,=", ";
            die "Static library not found: $libfile_static\nDirectories searched: @libdirs_static\n";
        }

    }
    $incdirs{$_} = 1 for (split ' ', <$ghcpkg>);
    close $ghcpkg;
    $graph{$pkgname} = \%pkg;

    add_to_graph($_) for @{$pkg{'depends'}};
}

my $root_pkg = $ARGV[0];
die "USAGE: $0 PKGNAME\n" unless $root_pkg;

add_to_graph $root_pkg;

local $\ = "\n";

system("mkdir", "-p", "lib");
for my $lib (@libfiles) {
  system("ln", "-sf", $lib, "lib/");
}

# system("mkdir", "-p", "lib_static");
# for my $lib (@libfiles_static) {
#     #print $lib;
#     system("ln", "-sf", $lib, "lib_static/");
# }

# open INCOPTS, ">", "incopts.txt";
# print INCOPTS "-I$_" for keys(%incdirs);
# close INCOPTS;

# my ($tsort_in, $tsort_out);
# open2($tsort_out, $tsort_in, "tsort");
# for my $pkgname (keys(%graph)) {
#   for (@{$graph{$pkgname}->{'depends'}}) {
#     printf $tsort_in "%s %s\n", $pkgname, $_;
#   }
# }
# close $tsort_in;

# open LDOPTS, ">", "ldopts.txt";

# print LDOPTS for keys(%ldopts);

# while (my $pkgname = <$tsort_out>) {
#   chomp $pkgname;
#   my %pkg = %{$graph{$pkgname}};
#   print LDOPTS "-l$_" for @{$pkg{'libs'}};
# }

# close LDOPTS;
