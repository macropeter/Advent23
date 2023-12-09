BEGIN {print "(";}
{
print "(";
print $0;
print ")";
}
END {print ")";}
