CLASS zcl_sys_generate_packages DEFINITION
  PUBLIC
  INHERITING FROM cl_xco_cp_adt_simple_classrun
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: main REDEFINITION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_package,
        name        TYPE sxco_package,
        parent      TYPE sxco_package,
        pkg_type    TYPE c LENGTH 1,
        swcomp      TYPE if_xco_gen_devc_s_fo_proprties=>tv_software_component,
        description TYPE as4text,
      END OF ty_package,
      tt_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY name.

    TYPES:
      BEGIN OF ty_track,
        name TYPE string,
        desc TYPE string,
      END OF ty_track,
      tt_tracks TYPE HASHED TABLE OF ty_track WITH UNIQUE KEY name.
    METHODS _prepare_packages
      RETURNING VALUE(rt_packages) TYPE tt_packages.
ENDCLASS.



CLASS zcl_sys_generate_packages IMPLEMENTATION.

  METHOD main.

    LOOP AT _prepare_packages( ) ASSIGNING FIELD-SYMBOL(<ls_package>).
      TRY.
*          out->write( |Creating a package - { <ls_package>-name }| ).
          out->write( <ls_package> ).

          DATA(lo_environment) = xco_generation=>environment->transported( 'GSSK900416' ).
          DATA(lo_put_operation) = lo_environment->for-devc->create_put_operation( ).
          DATA(lo_specification) = lo_put_operation->add_object( <ls_package>-name )->create_form_specification( ).

          lo_specification->set_short_description( <ls_package>-description ).
          lo_specification->properties->set_software_component( <ls_package>-swcomp
              )->set_application_component( ''
              )->set_transport_layer( 'SAP'
              )->set_package_type( SWITCH #( <ls_package>-pkg_type
                                    WHEN 'S' THEN xco_package=>type->structure
                                    ELSE xco_package=>type->development )
              )->set_translation_relevance( xco_package=>translation_relevance->no_translation
              )->set_adding_obj_not_possible( SWITCH #( <ls_package>-pkg_type
                                    WHEN 'S' THEN abap_true
                                    ELSE abap_false )
              )->set_encapsulated( abap_true
              )->set_record_object_changes( abap_true
              )->set_super_package( <ls_package>-parent ).
          lo_put_operation->execute( ).

          out->write( 'Okayyy' ).

        CATCH cx_xco_gen_put_exception INTO DATA(lx_xco_gen_put_exception).
          DATA(lt_messages) = lx_xco_gen_put_exception->if_xco_news~get_messages( ).
          LOOP AT lt_messages INTO DATA(ls_message).
            out->write( ls_message->get_text( ) ).
          ENDLOOP.
        CATCH cx_root INTO DATA(lx_root).
          out->write( lx_root->get_text( ) ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD _prepare_packages.
    DATA:
      lv_prefix TYPE string,
      lv_swcomp TYPE string,
      lv_desc   TYPE string,
      lv_parent TYPE string.

*        name        TYPE sxco_package,
*        parent      TYPE sxco_package,
*        swcomp      TYPE if_xco_gen_devc_s_fo_proprties=>tv_software_component,
*        description TYPE as4text,
    SELECT FROM ztsys_pkg AS a
      LEFT JOIN i_abappackage AS b
        ON  b~abappackage = a~pkg
      FIELDS
        a~pkg         AS name,
        a~parent_pkg  AS parent,
        a~pkg_type,
        a~description,
        a~swcomp
      WHERE b~abappackage IS NULL
      INTO CORRESPONDING FIELDS OF TABLE @rt_packages.
    SORT rt_packages BY parent name.
*    FINAL(lt_tracks) = VALUE tt_tracks(
*      ( name = 'FI' desc = 'Finance' )
*      ( name = 'CO' desc = 'Controlling' )
*      ( name = 'SD' desc = 'Sales & Distribution' )
*    ).
*
**    DO 1 TIMES.
*    DO 4 TIMES.
*      CASE sy-index.
*        WHEN 1.
*          lv_prefix = 'Z1'.
*          lv_swcomp = 'ZCUSTOM_DEVELOPMENT'.
*          lv_desc = 'Tier 1 (Dev Ext.)'.
*          lv_parent = 'ZTIER1'.
*
*        WHEN 2.
*          lv_prefix = 'Z2'.
*          lv_swcomp = 'HOME'.
*          lv_desc = 'Tier 2 (Cloud API Enablement)'.
*          lv_parent = 'ZTIER2'.
*
*        WHEN 3.
*          lv_prefix = 'Z3'.
*          lv_swcomp = 'HOME'.
*          lv_desc = 'Tier 3 (Classic Ext.)'.
*          lv_parent = 'ZTIER3'.
*
*        WHEN 4.
*          lv_prefix = 'ZKU'.
*          lv_swcomp = 'HOME'.
*          lv_desc = 'Tier 1 (Key-User Ext.)'.
*          lv_parent = 'ZKEYUSER'.
*
*      ENDCASE.
*
*      LOOP AT lt_tracks ASSIGNING FIELD-SYMBOL(<ls_track>).
*        INSERT VALUE ty_package(
*          name        = |{ lv_prefix }_{ <ls_track>-name }|
*          parent      = lv_parent
*          swcomp      = lv_swcomp
*          description = |{ lv_desc } - { <ls_track>-desc }|
*        ) INTO TABLE rt_packages.
*      ENDLOOP.
*
*    ENDDO.

  ENDMETHOD.

ENDCLASS.
