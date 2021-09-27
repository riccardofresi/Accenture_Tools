@AbapCatalog.sqlViewName: 'ZHANAVIEWVAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Hana View Variables'
define view Z_HANA_VIEW_VARIABLES

as select 
from zmetadatadom as lev1
left outer join zmetadatadom lev2 on lev1.node_id = lev2.parent_node_id
left outer join zmetadatadom lev3 on lev2.node_id = lev3.parent_node_id
left outer join zmetadatadom lev4 on lev3.node_id = lev4.parent_node_id
left outer join zmetadatadom lev5 on lev4.node_id = lev5.parent_node_id 
 {
lev1.objectname as CalculationView,
lev1.node_id as id1,

           max(case when lev2.node_name = 'id' then lev2.node_value else '' end) as id_variable,
           max(case when lev3.node_name = 'datatype' then lev3.node_value else '' end) as datatype,
           max(case when lev3.node_name = 'length' then lev3.node_value else '' end) as length,
           max(case when lev3.node_name = 'defaultValue' then lev3.node_value else '' end) as defaultValue,
           max(case when lev4.node_name = '#text' then lev4.node_value else '' end) as defaultExpression,
           max(case when lev4.node_name = 'procedureName' then lev5.node_value else '' end) as procedureName
} where      lev1.node_type = 1            -- get us all elements
and        lev1.node_name  = 'variable'
and lev1.objecttype = 'CALCVIEW'
group by lev1.objectname, lev1.node_id
