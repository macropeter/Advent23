BEGIN {print "("}
{
print "((";
$1=$2="";
gsub(/\|/, ")( ", $0);
print $0;
print "))"
}
END {print ")"}
