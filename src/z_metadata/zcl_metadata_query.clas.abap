class zcl_metadata_query definition
  public
  final
  create public .

  public section.
  interfaces: if_amdp_marker_hdb.

  types: p_query type c length 30.

types: begin of l_variable,
       ELTUID  type c length 25, OBJVERS  type c length 1, DEFTP  type c length 3,
       SUBDEFTP  type c length 3, REUSABLE type c length 1, ISCELL type c length 1, DEFAULTTEXT type c length 1,
       MAPNAME type c length 30, SOURCE_ELTUID  type c length 25, DEFAULTHINT type c length 30, DEFAULTHINTFLAG type c length 1,
       locate type i , end of l_variable,
       p_variable type standard table of l_variable with empty key.

types: begin of l_dimension,
       ELTUID  type c length 25, txtlg type c length 80, visibility type c length 25, iobjnm type c length 15, end of l_dimension,
       p_dimension type standard table of l_dimension with empty key.

types: begin of l_restrected,
       ELTUID  type c length 25, txtlg type c length 80, visibility type c length 25, iobjnm type c length 15,
       value type c length 50, lowflag type c length 5, opt type c length 5, end of l_restrected,
       p_restrected type standard table of l_restrected with empty key.

types: begin of l_formula,
       ELTUID  type c length 25, txtlg type c length 80, visibility type c length 25, Formula type c length 200,
        end of l_formula,
       p_formula type standard table of l_formula with empty key.

types: begin of l_formula_dependencies,
       ELTUID  type c length 25, text_formula type c length 80, visibility type c length 25, DEFTP  type c length 3,
       SUBDEFTP  type c length 3,DEFAULTHINT type c length 30, DEFAULTHINTFLAG type c length 1,stepnr type c length 5, substep type c length 5,
       o_flag type c length 1, operator type c length 25, text_operator type c length 100, iobjnm type c length 15,
       value type c length 50, lowflag type c length 5, opt type c length 5,
        end of l_formula_dependencies,
       p_formula_dependencies type standard table of l_formula_dependencies with empty key.

types: begin of l_all_dependecies,
       ELTUID  type c length 25, txtlg type c length 80, visibility type c length 25, iobjnm type c length 15,
        end of l_all_dependecies,
       p_all_dependecies type standard table of l_all_dependecies with empty key.

   class-methods get_query_details
        importing
            value(p_query) type p_query
        exporting
            value(p_variable) type p_variable
            value(p_dimension) type p_dimension
            value(p_restrected) type p_restrected
            value(p_formula) type p_formula
            value(p_formula_dependencies) type p_formula_dependencies
            value(p_all_dependecies) type p_all_dependecies.

  protected section.
  private section.
endclass.



class zcl_metadata_query implementation.

METHOD get_query_details BY DATABASE PROCEDURE
                             FOR HDB
                             LANGUAGE SQLSCRIPT
                             OPTIONS read-only USING RSZELTDIR RSZELTXREF RSZSELECT RSZRANGE RSZCALC RSZELTTXT RSZELTPROP RSDIOBJT.


 QUERY = select
     MAPNAME AS QUERY_ID,
     A.ELTUID AS QUERY_GUID
    -- TXTLG AS QUERY_TEXT
from RSZELTDIR A
where DEFTP = 'REP'
AND A.OBJVERS = 'A'
AND MAPNAME = :p_query;

XREF_1 = select xref.SELTUID, xref.TELTUID, 1 AS LEVEL from RSZELTXREF xref inner join :query query on query.query_guid = xref.SELTUID where objvers = 'A';
XREF_2 = SELECT xref.SELTUID, xref.TELTUID, 2 AS LEVEL FROM RSZELTXREF xref inner join :XREF_1 on :XREF_1.TELTUID = xref.SELTUID where objvers = 'A';
XREF_3 = SELECT xref.SELTUID, xref.TELTUID, 3 AS LEVEL FROM RSZELTXREF xref inner join :XREF_2 on :XREF_2.TELTUID = xref.SELTUID where objvers = 'A';

ELTXREF = select distinct guid from (
select distinct SELTUID as guid from (
SELECT * FROM :XREF_1
UNION SELECT * FROM :XREF_2
UNION SELECT * FROM :XREF_3
)
union select distinct TELTUID as guid from (
SELECT * FROM :XREF_1
UNION SELECT * FROM :XREF_2
UNION SELECT * FROM :XREF_3
)
);

ELTDIR =  select A.*,
      case when locate(defaulthint,'__') = 0 then locate(defaulthint,'-') else locate(defaulthint,'__') + 1 end as Locate
from :ELTXREF INNER JOIN RSZELTDIR A ON :ELTXREF.GUID = A.ELTUID
where
A.OBJVERS = 'A' ;

--SELECT * FROM :ELTDIR;

compid = SELECT * FROM :ELTDIR where deftp = 'REP' OR DEFTP = 'VAR';

p_variable = SELECT * FROM :COMPID where deftp = 'VAR';


TSELECT = select
     A.*, case when locate(iobjnm,'__') = 0 then locate(iobjnm,'-') else locate(iobjnm,'__') + 1 end as Locate
from :ELTXREF INNER JOIN RSZSELECT A ON :ELTXREF.GUID = A.ELTUID
where
A.OBJVERS = 'A' ;

--SELECT * FROM :TSELECT;

RANGE = select
     a.*, case when locate(iobjnm,'__') = 0 then locate(iobjnm,'-') else locate(iobjnm,'__') + 1 end as Locate
from :ELTXREF INNER JOIN RSZRANGE A ON :ELTXREF.GUID = A.ELTUID
where
A.OBJVERS = 'A' ;

--SELECT * FROM :RANGE;


CALC = select
     a.*
from :ELTXREF INNER JOIN RSZCALC A ON :ELTXREF.GUID = A.ELTUID
where
A.OBJVERS = 'A' ;

calc_norm = select ELTUID, stepnr, 1 as substep, opera, o1flg as O_FLAG,oper1 AS OPERATOR from :calc
            union
            select ELTUID, stepnr, 2 as substep, '' as opera, o2flg as O_FLAG,oper2 AS OPERATOR from :calc where o2flg != '';

--SELECT * FROM :CALC;

ELTTXT = select
     a.*
from :ELTXREF INNER JOIN RSZELTTXT A ON :ELTXREF.GUID = A.ELTUID
where
A.OBJVERS = 'A' AND LANGU = 'E';

--SELECT * FROM :ELTTXT;

ELTPROP = select
     a.*
from :ELTXREF INNER JOIN RSZELTPROP A ON :ELTXREF.GUID = A.ELTUID
where
A.OBJVERS = 'A';

--SELECT * FROM :ELTPROP;


HIDDEN = SELECT :ELTPROP.ELTUID,
                CASE WHEN HIDDEN = 'X'
                    THEN 'HIDE'
                    ELSE CASE WHEN HIDDEN = 'Y'
                        THEN 'Hide (Can Be Shown)'
                        ELSE 'Display'
                        END
                END AS Visibility
                FROM :ELTPROP INNER JOIN :ELTDIR ON :ELTPROP.ELTUID = :ELTDIR.ELTUID;

--SELECT * FROM :HIDDEN;

DEFAULTHINT = select distinct DEFAULTHINT, substr_after(DEFAULTHINT, '__') as InfoObject from :ELTDIR where DEFAULTHINT != '';

InfoObject = select DEFAULTHINT, IOBJNM, TXTLG from RSDIOBJT inner join :DEFAULTHINT on IOBJNM = InfoObject where langu = 'E';

TEXT = SELECT DISTINCT :ELTDIR.ELTUID, coalesce(coalesce(coalesce(:ELTTXT.TXTLG, text2.TXTLG) ,:InfoObject.TXTLG), :ELTDIR.DEFAULTHINT ) as TXTLG
                FROM :ELTDIR
                left outer JOIN :ELTTXT  ON :ELTTXT.ELTUID = :ELTDIR.ELTUID
                left outer join :ELTTXT text2 on text2.ELTUID = :ELTDIR.DEFAULTHINT
                left outer join :InfoObject on :ELTDIR.DEFAULTHINT = :InfoObject.DEFAULTHINT;
--select * from :text;

SEL =
select
:ELTDIR.ELTUID,
    txtlg,
    :hidden.visibility,
:ELTDIR.subdeftp,
CASE WHEN :RANGE.IOBJNM = '1KYFNM'
    THEN right(:ELTDIR.defaulthint, length(:ELTDIR.defaulthint) - :ELTDIR.locate)
    ELSE right(coalesce(:TSELECT.IOBJNM,:RANGE.IOBJNM ), length(coalesce(:TSELECT.IOBJNM,:RANGE.IOBJNM )) - coalesce(:TSELECT.locate,:RANGE.locate ))
    --substr_after(coalesce(:TSELECT.IOBJNM,:RANGE.IOBJNM ), '__')
    END AS IOBJNM,
COALESCE(:COMPID.MAPNAME, LOW) AS VALUE,
LOWFLAG ,
CASE OPT
    WHEN 'EQ' THEN
        CASE WHEN SIGN = 'I' THEN '=' ELSE '<>' END
    WHEN 'GE' THEN '>='
    WHEN 'LE' THEN '<='
    WHEN 'GT' THEN '>'
    WHEN 'LT' THEN '<'
    ELSE '' END AS OPT
from :ELTDIR
LEFT OUTER JOIN :RANGE ON :RANGE.ELTUID = :ELTDIR.ELTUID
LEFT OUTER JOIN :COMPID ON low = :COMPID.ELTUID
LEFT OUTER JOIN :TSELECT ON :TSELECT.ELTUID = :ELTDIR.ELTUID and   :RANGE.ELTUID is null
LEFT OUTER JOIN :TEXT ON :ELTDIR.ELTUID = :TEXT.ELTUID
left outer join :hidden on :ELTDIR.ELTUID = :hidden.ELTUID
where :ELTDIR.DEFTP = 'SEL' ;

p_dimension = SELECT ELTUID, txtlg , visibility, IOBJNM FROM :SEL where suBdeftp = 'CHA';

p_restrected = SELECT ELTUID, txtlg , visibility, IOBJNM, value, lowflag, opt FROM :SEL where suBdeftp != 'CHA';

formula =
select :ELTDIR.ELTUID,
:ELTDIR.objvers,
text_formula.txtlg as Text_Formula,
:ELTDIR.deftp,
:ELTDIR.subdeftp,
:ELTDIR.defaulthint,
:ELTDIR.defaulthintflag,
stepnr,
substep,
o_flag,
operator,
text_operator.txtlg as text_operator,
opera,
case when text_operator.txtlg is null
    then opera || ' '
    else  text_operator.txtlg || ' ' || opera || ' '
    end as string ,
    Visibility
from :ELTDIR
left outer join :calc_norm on :ELTDIR.ELTUID = :calc_norm.ELTUID
LEFT OUTER JOIN :TEXT as text_formula ON :ELTDIR.ELTUID = text_formula.ELTUID
LEFT OUTER JOIN :TEXT as text_operator ON OPERATOR = text_operator.ELTUID
left outer join :hidden on :ELTDIR.ELTUID = :hidden.ELTUID
where (:ELTDIR.DEFTP = 'CKF' or :ELTDIR.DEFTP = 'FML')
order by  :ELTDIR.ELTUID, stepnr, substep;



p_formula = select ELTUID, Text_Formula as txtlg,Visibility, string_agg(string, ' ') as Formula from :formula group by ELTUID, Text_Formula, visibility;

formula_dependencies = SELECT :ELTDIR.ELTUID,
text_formula.txtlg as Text_Formula,
:hidden.Visibility,
:ELTDIR.deftp,
:ELTDIR.subdeftp,
:ELTDIR.defaulthint,
:ELTDIR.defaulthintflag,
stepnr,
substep,
o_flag,
operator,
text_operator.txtlg as text_operator,
iobjnm,
value,
lowflag,
opt
 from :ELTDIR
left outer join :calc_norm on :ELTDIR.ELTUID = :calc_norm.ELTUID
LEFT OUTER JOIN :TEXT as text_formula ON :ELTDIR.ELTUID = text_formula.ELTUID
LEFT OUTER JOIN :TEXT as text_operator ON OPERATOR = text_operator.ELTUID
LEFT OUTER JOIN :SEL ON :calc_norm.OPERATOR = :SEL.ELTUID
left outer join :hidden on :ELTDIR.ELTUID = :hidden.ELTUID
where (:ELTDIR.DEFTP = 'CKF' or :ELTDIR.DEFTP = 'FML')
AND ( O_FLAG = 'B' OR O_FLAG IS NULL );

p_formula_dependencies = select * from :formula_dependencies;

p_all_dependecies = select ELTUID,
    txtlg,
    visibility,
    iobjnm
    from :SEL
    union
    select ELTUID,
Text_Formula as txtlg,
Visibility,
iobjnm from :formula_dependencies;


endmethod.



endclass.
