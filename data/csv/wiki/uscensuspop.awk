#! /usr/local/bin/gawk -f

BEGIN { FS="," 
#    printf "%s\n",place

}

 $1 ~ place {
     printf "%s\n",$1
    #printf "NF=%d\n",NF 

printf "{{USCensusPop\n"
for(i=NF;i>=5; i--)
{
    if($i>0)
    {
	#printf '%d',$i
	printf "|%d= %d\n",(year+(NF-i)*10),$i
    }
}
    printf "|estyear=2014\n"
    printf "|estimate=%d\n",$4
    printf "|estref=<ref name=\"USCensusEst2014\">{{cite web|url=http://www.census.gov/popest/data/cities/totals/2014/SUB-EST2014.html|title=Annual Estimates of the Resident Population for Incorporated Places: April 1, 2010 to July 1, 2014|accessdate=June 4, 2015}}</ref>\n"
    printf "|footnote=<center>U.S. Decennial Census<ref name=\"DecennialCensus\">{{cite web|url=http://www.census.gov/prod/www/decennial.html|title=Census of Population and Housing|publisher=Census.gov|accessdate=June 4, 2015}}</ref></center>\n"
    printf "}}\n"



}

