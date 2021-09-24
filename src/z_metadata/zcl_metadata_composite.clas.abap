class zcl_metadata_composite definition
  public
  final
  create public .

  public section.
  interfaces: if_amdp_marker_hdb.

  types: p_calcview type c length 30.

  types: begin of l_dom,
  " unique id of the node
    node_id  type i
  " id of the parent node
  , parent_node_id   type i
  " dom node type constant: 1=element, 2=attribute, 3=text, 4=cdata, 5=entityref, 6=entity, 7=processing instruction, 8=comment, 9=document, 10=document type, 11=document fragment, 12=notation
  , node_type         type int2
  " dom node name: tagname for element,
                   "attribute name for attribute,
                   "target for processing instruction,
                   "document type name for document type,
                   "#text" for text and cdata,
                   "#comment" for comment,
                   "#document" for document,
                   "#document-fragment" for document fragment.
  , node_name         type c length 64
  " dom node value: text for text, comment, and cdata nodes, data for processing instruction node, null otherwise.
  , node_value        type string
  " character position of token
  , pos               type i
  " lenght of token
  , len               type i
  , end of l_dom,
  begin of l_error,
    error_code        type i
  , error_message     type c length 255
  , position          type i
  , node_name         type c length 255
  , end of l_error,

  p_dom type standard table of l_dom with empty key,
  p_error type standard table of l_error with empty key.



types: begin of l_rank_details,
       id_rank  type c length 255, id_input  type c length 255, id_field  type c length 255, source  type c length 255, end of l_rank_details,
       p_rank_details type standard table of l_rank_details with empty key.

types: begin of l_details,
       target type c length 255, source type c length 255, entity type c length 255,entityType type c length 20, end of l_details,
       p_details type standard table of l_details with empty key.

   class-methods get_xml_from_composite
        importing
            value(p_calcview) type p_calcview
        exporting
            value(p_xml) type string.

   class-methods p_decode_xml_entities
        importing value(p_encoded_text) type string
        exporting value(p_decoded_text) type string.

   class-methods p_parse_xml
        importing
         value(p_xml) type string
        exporting
         value(p_dom) type p_dom
         value(p_error) type p_error
         value(p_strip_empty_text) type i.

   class-methods get_details
        importing
         value(tab_dom) type p_dom
        exporting
         value(p_details) type p_details.

  protected section.
  private section.
endclass.



class zcl_metadata_composite implementation.

METHOD get_xml_from_composite BY DATABASE PROCEDURE
                             FOR HDB
                             LANGUAGE SQLSCRIPT
                             OPTIONS read-only USING rsohcpr.


SELECT  cast(BINTOSTR(cast(xml_ui as binary)) as varchar) XML into p_xml
from rsohcpr
WHERE
--PACKAGE_ID    = 'my.package.name'
--AND
HCPRNM   = :p_calcview
--OBJECT_NAME = 'AAATEST001'
AND     objvers = 'A';

endmethod.

METHOD p_decode_xml_entities BY DATABASE PROCEDURE
                             FOR HDB
                             LANGUAGE SQLSCRIPT
                             OPTIONS read-only.

  declare i integer default 1;
  declare n integer default length(p_encoded_text);
  declare v_from_position integer;
  declare v_to_position integer;
  declare v_encoded_text nclob default p_encoded_text;
  declare v_text nclob default '';
  declare v_token nvarchar(12);

  v_encoded_text =
    replace(
      replace(
        replace(
          replace(v_encoded_text,
            '&gt;', '>')
          , '&lt;', '<')
          , '&quot;', '"')
          , '&apos;', '''')
  ;

  while i <= n do
    select  locate_regexpr('&(#(\d+|[xX]?[\dA-Za-z]+));' in v_encoded_text from i)
    into    v_from_position
    from    dummy
    ;
    if v_from_position = 0 then
      v_text = v_text || substr(v_encoded_text, i);
      i = n + 1;
    else
      v_text = v_text || substr(v_encoded_text, i, v_from_position - i);
      v_to_position = locate(v_encoded_text, ';', i);
      v_token = substr(v_encoded_text, v_from_position + 1, v_to_position - v_from_position - 1);
      if substr(v_token, 1, 1) = '#' then
        if substr(v_token, 2, 1) = 'x' then
          v_text = v_text || bintostr(hextobin(substr(v_token, 3)));
        else
          v_text = v_text || nchar(cast(substr(v_token, 2) as integer));
        end if;
      elseif v_token = 'amp' then
        v_text = v_text || '&';
      elseif v_token = 'apos' then
        v_text = v_text || '''';
      elseif v_token = 'lt' then
        v_text = v_text || '<';
      elseif v_token = 'gt' then
        v_text = v_text || '>';
      elseif v_token = 'quot' then
        v_text = v_text || '"';
      else
        signal sql_error_code 10000
        set message_text = 'Unrecognized entity '||v_token;
      end if;
      i = v_to_position + 1;
    end if
    ;
  end while
  ;
  v_text = replace(v_text, '&amp;', '&');
  p_decoded_text = v_text;


endmethod.

method p_parse_xml BY DATABASE PROCEDURE
                             FOR HDB
                             LANGUAGE SQLSCRIPT
                             OPTIONS read-only using ZCL_METADATA_COMPOSITE=>P_DECODE_XML_ENTITIES.

  -- default regexp flag: s: . includes newline; m: ^ and $ match start/end of input (not of line)
  declare RX_FLAG     CONSTANT nchar(2)       default 'sm';
  -- less than
  declare RX_LT       CONSTANT nchar(1)       default '<';
  declare RX_LT_LEN   CONSTANT tinyint        default length(RX_LT);
  -- greater than
  declare RX_GT       CONSTANT nchar(1)       default '>';
  -- single quote
  declare RX_APOS     CONSTANT nchar(8)       default '''';
  -- double quote
  declare RX_QUOT     CONSTANT nchar(8)       default '"';
  -- name start char
  declare RX_NSCHAR   CONSTANT nchar(27)      default ':_A-Za-z\xC0-\xD6\xD8-\xF6';
  -- name char
  declare RX_NCHAR    CONSTANT nchar(38)      default '\-\.'||RX_NSCHAR||'0-9\xB7';
  -- name: one or more word characters. (TODO: xml probably allows more chars than \w)
  declare RX_NAME     CONSTANT nchar(70)      default '['||RX_NSCHAR||']['||RX_NCHAR||']*';
  -- qualified name: name with optional prefix
  declare RX_QNAME    CONSTANT nchar(146)     default '(('||RX_NAME||':)?'||RX_NAME||')';
  -- whitespace
  declare RX_WS       CONSTANT nchar(3)       default '\s+';
  -- optional whitespace
  declare RX_OPTWS    CONSTANT nchar(3)       default '\s*';
  -- single quoted string: single quote, followed by anything byt a single quote or a left angle parenthesis, followed by single quote
  declare RX_SQSTR    CONSTANT nchar(8)       default RX_APOS||'[^'||RX_APOS||RX_LT||']*'||RX_APOS;
  -- double quoted string: double quote, followed by anything but a double quote or a left angle parenthesis, followed by double quote
  declare RX_DQSTR    CONSTANT nchar(8)       default RX_QUOT||'[^'||RX_QUOT||RX_LT||']*'||RX_QUOT;
  -- quoted string: either a double or a single quoted string
  declare RX_QSTR     CONSTANT nchar(19)      default '('||RX_SQSTR||'|'||RX_DQSTR||')';
  -- attribute: whitespace, qname, optional whitespace, equals sign, optional whitespace, quoted string.
  declare RX_ATT      CONSTANT nchar(178)      default '('||RX_WS||RX_QNAME||RX_OPTWS||'='||RX_OPTWS||RX_QSTR||')';
  -- literal question mark (used in pi)
  declare RX_Q        CONSTANT nchar(2)       default '\?';
  -- start pi
  declare RX_SPI      CONSTANT nchar(3)       default RX_LT||RX_Q;
  -- end pi
  declare RX_EPI      CONSTANT nchar(3)       default RX_Q||RX_GT;
  -- processing instruction: name, mandatory whitespace, followed by anything that is not a end pi delimiter
  declare RX_PI       CONSTANT nchar(193)      default RX_SPI||'('||RX_NAME||')('||RX_WS||'.*(?<!'||RX_EPI||'))'||RX_EPI;
  -- dash
  declare RX_DASH     CONSTANT nchar(1)       default '-';
  -- dashdash
  declare RX_DASHDASH CONSTANT nchar(2)       default RX_DASH||RX_DASH;
  -- start comment
  declare RX_SCOMM    CONSTANT nchar(4)       default RX_LT||'!'||RX_DASHDASH;
  declare RX_SCOMM_LEN CONSTANT tinyint       default length(RX_SCOMM);
  -- end comment
  declare RX_ECOMM    CONSTANT nchar(3)       default RX_DASHDASH||RX_GT;
  declare RX_ECOMM_LEN CONSTANT tinyint       default length(RX_ECOMM);
  -- no dash
  declare RX_NODASH   CONSTANT nchar(4)       default '[^-]';
  -- comment:
  declare RX_COMMENT  CONSTANT nchar(20)      default RX_SCOMM||'('||RX_NODASH||'|'||RX_DASH||RX_NODASH||')*'||RX_ECOMM;
  -- start cdata:
  declare RX_SCDATA   CONSTANT nchar(11)      default RX_LT||'!\[CDATA\[';
  declare RX_SCDATA_LEN CONSTANT tinyint      default length(RX_SCDATA);
  -- end cdata:
  declare RX_ECDATA   CONSTANT nchar(5)       default '\]\]'||RX_GT;
  declare RX_ECDATA_LEN CONSTANT tinyint      default length(RX_ECDATA);
  -- cdata
  declare RX_CDATA    CONSTANT nchar(28)      default RX_SCDATA||'.*(?<!'||RX_ECDATA||')'||RX_ECDATA;
  -- external id
  declare RX_EXTID    CONSTANT nchar(61)      default '((SYSTEM|PUBLIC'||RX_WS||RX_QSTR||')'||RX_WS||RX_QSTR||')';
  -- doctype start
  declare RX_SDOCTYPE CONSTANT nchar(9)       default RX_LT||'!DOCTYPE';
  -- doctype
  declare RX_DOCTYPE  CONSTANT nchar(155)     default RX_SDOCTYPE||RX_WS||'('||RX_NAME||')('||RX_WS||RX_EXTID||')?'||RX_OPTWS||RX_GT;
  declare RX_DOCTYPE_LEN CONSTANT tinyint     default length(RX_SDOCTYPE);
  -- opening tag
  declare RX_STAG     CONSTANT nchar(334)     default RX_LT||RX_QNAME||'('||RX_ATT||'*)'||RX_OPTWS||'/?'||RX_GT;
  -- opening tag
  declare RX_CTAG     CONSTANT nchar(149)     default RX_LT||'/'||RX_QNAME||RX_GT;
  -- text: any content between > and <
  declare RX_TEXT     CONSTANT nchar(16)      default '(?<='||RX_GT||')[^'||RX_LT||']+(?='||RX_LT||')';
  --
  declare REGXP       CONSTANT nchar(901)     default RX_PI
                                               ||'|'||RX_COMMENT
                                               ||'|'||RX_CDATA
                                               ||'|'||RX_DOCTYPE
                                               ||'|'||RX_STAG
                                               ||'|'||RX_CTAG
                                               ||'|'||RX_TEXT
  ;
  declare CLOSE_ELEMENT               CONSTANT tinyint default 0;  -- pseudo-nodetype used to signal closing element

  -- DOM node types.
  declare ELEMENT_NODE                CONSTANT tinyint default 1;
  declare ATTRIBUTE_NODE              CONSTANT tinyint default 2;
  declare TEXT_NODE                   CONSTANT tinyint default 3;
  declare CDATA_SECTION_NODE          CONSTANT tinyint default 4;
  declare ENTITY_REFERENCE_NODE       CONSTANT tinyint default 5;
  declare ENTITY_NODE                 CONSTANT tinyint default 6;
  declare PROCESSING_INSTRUCTION_NODE CONSTANT tinyint default 7;
  declare COMMENT_NODE                CONSTANT tinyint default 8;
  declare DOCUMENT_NODE               CONSTANT tinyint default 9;
  declare DOCUMENT_TYPE_NODE          CONSTANT tinyint default 10;
  declare DOCUMENT_FRAGMENT_NODE      CONSTANT tinyint default 11;
  declare NOTATION_NODE               CONSTANT tinyint default 12;

  declare v_node_id integer default 0;
  declare v_element_stack integer array;
  declare v_parent_node_id integer default 0;

  declare v_node_type tinyint;
  declare v_node_name nvarchar(64);
  declare v_node_value nclob;
  declare v_chars char(12);
  declare v_index integer default 1;
  declare v_length integer;
  declare v_end   integer default length(:p_xml);
  declare v_token nclob;

  declare v_atts  nclob;
  declare v_atts_index  integer;
  declare v_atts_length integer;
  declare v_atts_end    integer;
  declare v_att         nclob;
  declare v_att_name    nvarchar(64);
  declare v_att_value   nclob;

  declare v_row_num integer default 1;
  declare v_node_ids integer array;
  declare v_parent_node_ids integer array;
  declare v_node_types tinyint array;
  declare v_node_names nvarchar(64) array;
  declare v_node_values nclob array;
  declare v_positions integer array;
  declare v_lengths integer array;

  declare exit handler for sqlexception
    begin
      p_error = select
        ::SQL_ERROR_CODE    error_code
      , ::SQL_ERROR_MESSAGE error_message
      , v_index             position
      , v_node_name         node_name
      from dummy;
      p_dom = unnest(
        :v_node_ids
      , :v_parent_node_ids
      , :v_node_types
      , :v_node_names
      , :v_node_values
      , :v_positions
      , :v_lengths
      ) as dom (
        node_id
      , parent_node_id
      , node_type
      , node_name
      , node_value
      , pos
      , len
      );
    end;

  p_strip_empty_text = 1;

  v_element_stack[v_row_num] = v_node_id;

  v_node_ids[v_row_num] = v_node_id;
  v_parent_node_ids[v_row_num] = null;
  v_node_types[v_row_num] = DOCUMENT_NODE;
  v_node_names[v_row_num] = '#document';
  v_node_values[v_row_num] = null;
  v_positions[v_row_num] = 1;
  v_lengths[v_row_num] = v_end;

  while v_index < v_end do

    select  substr_regexpr(REGXP flag RX_FLAG in :p_xml from v_index)
    into    v_token
    from    dummy;

    v_length = length(v_token);
    v_node_id = v_node_id + 1;
    v_parent_node_id = :v_element_stack[CARDINALITY(:v_element_stack)];
    v_node_name = null;
    v_node_type = null;
    v_node_value = null;
    v_atts = null;

    if v_token is null then
      -- check for whitespace trailing the document
      select substr_regexpr('\s+$' flag RX_FLAG in :p_xml from v_index)
      into v_token
      from dummy;
      if length(v_token) > 0 then
        v_index = v_index + length(v_token);
        v_node_type = 0;
      else
        signal sql_error_code 10000
          set message_text = 'No token found at '||cast(v_index as varchar(12));
      end if;
    elseif left(v_token, RX_LT_LEN) = RX_LT then
      if substr(v_token, 2, 1) = '?' then
        v_node_type = PROCESSING_INSTRUCTION_NODE;
        select  substr_regexpr(RX_PI flag RX_FLAG in v_token group 1)
        ,       substr_regexpr(RX_PI flag RX_FLAG in v_token group 2)
        into    v_node_name
        ,       v_node_value
        from    dummy;
        v_atts = v_node_value;
      elseif left(v_token, RX_SCOMM_LEN) = RX_SCOMM then
        v_node_type = COMMENT_NODE;
        v_node_name = '#comment';
        v_node_value = substr(v_token, RX_SCOMM_LEN + 1, v_length - RX_SCOMM_LEN - RX_ECOMM_LEN);
      elseif left(v_token, RX_SCDATA_LEN) = RX_SCDATA then
        v_node_type = CDATA_SECTION_NODE;
        v_node_name = '#text';
        v_node_value = substr(v_token, RX_SCDATA_LEN + 1, v_length - RX_SCDATA_LEN - RX_ECDATA_LEN);
      elseif left(v_token, RX_DOCTYPE_LEN) = RX_SDOCTYPE then
        v_node_type = DOCUMENT_TYPE_NODE;
        select  substr_regexpr(RX_DOCTYPE flag RX_FLAG in v_token group 1)
        into    v_node_name
        from    dummy;
      elseif substr(v_token, 2, 1) = '/' then
        v_node_type = CLOSE_ELEMENT;
        v_node_id = v_node_id - 1;
        v_element_stack = trim_array(:v_element_stack, 1);
      else
        v_node_type = ELEMENT_NODE;
        select  substr_regexpr(RX_STAG in v_token group 1)
        ,       substr_regexpr(RX_STAG in v_token group 3)
        into    v_node_name
        ,       v_atts
        from    dummy;
        v_chars = substr(v_token, v_length - 1, 1);
        if v_chars != '/' then
           v_element_stack[CARDINALITY(:v_element_stack) + 1] = v_node_id;
        end if;
      end if;
    else
      v_node_type = TEXT_NODE;
      v_node_name = '#text';
      call "ZCL_METADATA_COMPOSITE=>P_DECODE_XML_ENTITIES"(
        v_token
      , v_node_value
      );
    end if;

    -- lose non-significant whitespace.
    if p_strip_empty_text != 0 and v_node_type = TEXT_NODE then
      select case count(*) when 1 then 0 else v_node_type end
      ,      v_node_id - count(*)
      into   v_node_type, v_node_id
      from dummy
      where replace_regexpr ('^\s+$' flag RX_FLAG in v_token with '') = '';
    end if;

    if v_node_type > 0 then
      v_row_num = v_row_num + 1;

      v_node_ids[v_row_num] = v_node_id;
      v_parent_node_ids[v_row_num] = v_parent_node_id;
      v_node_types[v_row_num] = v_node_type;
      v_node_names[v_row_num] = v_node_name;
      v_node_values[v_row_num] = v_node_value;
      v_positions[v_row_num] = v_index;
      v_lengths[v_row_num] = v_length;

      if not v_atts is null then
        v_parent_node_id = v_node_id;
        v_atts_index = 1;
        v_atts_end = length(v_atts);

        while v_atts_index < v_atts_end do
          select  substr_regexpr(RX_ATT flag RX_FLAG in v_atts from v_atts_index group 1)
          ,       substr_regexpr(RX_ATT flag RX_FLAG in v_atts from v_atts_index group 2)
          ,       substr_regexpr(RX_ATT flag RX_FLAG in v_atts from v_atts_index group 4)
          into    v_att, v_att_name, v_att_value
          from    dummy;
          v_atts_length = length(v_att);

          if v_att_name is null then
            signal sql_error_code 10000
            set message_text = 'No attribute found in '||v_atts||' at index '||cast(v_atts_index as varchar(12));
          else
            v_node_id = v_node_id + 1;

            call "ZCL_METADATA_COMPOSITE=>P_DECODE_XML_ENTITIES"(
              substr(v_att_value, 2, length(v_att_value) - 2)
            , v_att_value
            );

            v_row_num = v_row_num + 1;

            v_node_ids[v_row_num] = v_node_id;
            v_parent_node_ids[v_row_num] = v_parent_node_id;
            v_node_types[v_row_num] = ATTRIBUTE_NODE;
            v_node_names[v_row_num] = v_att_name;
            v_node_values[v_row_num] = v_att_value;
            v_positions[v_row_num] = v_index + v_atts_index;
            v_lengths[v_row_num] = v_atts_length;
          end if;
          v_atts_index = v_atts_index + v_atts_length;
        end while;
      end if;
    end if;
    v_index = v_index + v_length;
  end while;

  p_dom = unnest(
    :v_node_ids
  , :v_parent_node_ids
  , :v_node_types
  , :v_node_names
  , :v_node_values
  , :v_positions
  , :v_lengths
  ) as dom (
    node_id
  , parent_node_id
  , node_type
  , node_name
  , node_value
  , pos
  , len
  );

endmethod.

method get_details by database procedure for hdb language sqlscript options read-only.

entity = select   lev1.parent_node_id, lev1.node_id, cast(BINTOSTR(cast(lev2.node_value as binary)) as varchar) as Entity
from :tab_dom lev1
left outer join :tab_dom lev2 on lev1.node_id = lev2.parent_node_id
left outer join :tab_dom lev3 on lev2.node_id = lev3.parent_node_id
left outer join :tab_dom lev4 on lev3.node_id = lev4.parent_node_id
left outer join :tab_dom lev5 on lev4.node_id = lev5.parent_node_id
where      lev1.node_type = 257           -- get us all elements
and        lev1.node_name in ('entity');

infoobject = select  lev1.parent_node_id, lev1.node_id,
max(case when lev2.node_name = 'name' then cast(BINTOSTR(cast(lev2.node_value as binary)) as varchar) end) as Target,
max(case when lev3.node_name = 'globalElementName' then cast(BINTOSTR(cast(lev3.node_value as binary)) as varchar) end) as InfoObject
from :tab_dom lev1
left outer join :tab_dom lev2 on lev1.node_id = lev2.parent_node_id
left outer join :tab_dom lev3 on lev2.node_id = lev3.parent_node_id
left outer join :tab_dom lev4 on lev3.node_id = lev4.parent_node_id
left outer join :tab_dom lev5 on lev4.node_id = lev5.parent_node_id
where      lev1.node_type = 257           -- get us all elements
and        lev1.node_name in ('element')
group by lev1.parent_node_id, lev1.node_id;

mapping = select   lev1.parent_node_id, lev1.node_id, Entity,
max(case when lev2.node_name = 'targetName' then cast(BINTOSTR(cast(lev2.node_value as binary)) as varchar) end) as Target,
max(case when lev2.node_name = 'sourceName' then cast(BINTOSTR(cast(lev2.node_value as binary)) as varchar) end) as Source
from :tab_dom lev1
left outer join :tab_dom lev2 on lev1.node_id = lev2.parent_node_id
left outer join :entity on lev1.parent_node_id = :entity.parent_node_id
where      lev1.node_type = 257           -- get us all elements
and        lev1.node_name in ('mapping')
group by lev1.parent_node_id, lev1.node_id, Entity;


p_details = select
coalesce(InfoObject, a.target) as target,
Source,
substr_after(replace(Entity ,'.calculationview#/',''), '/') as Entity,
case when locate( Entity,'.calculationview#/') > 0 then 'CALCULATIONVIEW' ELSE 'OTHER' END AS EntityType
from :mapping a
left outer join :infoobject b on a.target = b.target
order by coalesce(InfoObject, a.target) asc;



endmethod.

endclass.
