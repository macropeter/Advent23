BEGIN {print "(";}
{
    print "(";
    $1="";
    print $0;
    print ")";
}
END {print ")";}
