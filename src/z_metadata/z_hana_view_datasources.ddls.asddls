@AbapCatalog.sqlViewName: 'ZHANAVIEWDSS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Hana View Datasources'
define view Z_HANA_VIEW_DATASOURCES with parameters p_calc_view : ABAP.char( 64 )  
as select 
from z_hana_object(CV: :p_calc_view) as lev1
left outer join z_hana_object(CV: :p_calc_view) lev2 on lev1.node_id = lev2.parent_node_id
left outer join z_hana_object(CV: :p_calc_view) lev3 on lev2.node_id = lev3.parent_node_id 
 {
$parameters.p_calc_view as CalculationView,
lev1.node_id ,
           max(case when lev2.node_name = 'id' then lev2.node_value else '' end) as id_datasource,
           max(case when lev2.node_name = 'type' then lev2.node_value else '' end) as type,
           max(case when lev3.node_name = '#text' then lev3.node_value else '' end) as CalculationView_Source,
           max(case when lev3.node_name = 'schemaName' then lev3.node_value else '' end) as schemaName,
           max(case when lev3.node_name = 'columnObjectName' then lev3.node_value else '' end) as columnObjectName

} where      lev1.node_type = 257            -- get us all elements
and        lev1.node_name  = 'DataSource'
group by lev1.node_id
