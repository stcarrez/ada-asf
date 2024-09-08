-----------------------------------------------------------------------
--  Facelet Tests - Unit tests for ASF.Views.Facelet
--  Copyright (C) 2009, 2010, 2011, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with EL.Objects;
with ASF.Converters;
with ASF.Validators;
with ASF.Contexts.Facelets;
package body ASF.Views.Facelets.Tests is

   use ASF.Contexts.Facelets;

   type Facelet_Context is new ASF.Contexts.Facelets.Facelet_Context with null record;

   --  Get a converter from a name.
   --  Returns the converter object or null if there is no converter.
   overriding
   function Get_Converter (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Converters.Converter_Access;

   --  Get a validator from a name.
   --  Returns the validator object or null if there is no validator.
   overriding
   function Get_Validator (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Validators.Validator_Access;

   --  ------------------------------
   --  Get a converter from a name.
   --  Returns the converter object or null if there is no converter.
   --  ------------------------------
   overriding
   function Get_Converter (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Converters.Converter_Access is
      pragma Unreferenced (Context, Name);
   begin
      return null;
   end Get_Converter;

   --  ------------------------------
   --  Get a validator from a name.
   --  Returns the validator object or null if there is no validator.
   --  ------------------------------
   overriding
   function Get_Validator (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Validators.Validator_Access is
      pragma Unreferenced (Context, Name);
   begin
      return null;
   end Get_Validator;

   --  ------------------------------
   --  Set up performed before each test case
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;

   --  ------------------------------
   --  Tear down performed after each test case
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
   begin
      null;
   end Tear_Down;

   --  ------------------------------
   --  Test loading of facelet file
   --  ------------------------------
   procedure Test_Load_Facelet (T : in out Test) is
      Factory    : ASF.Views.Facelets.Facelet_Factory;
      Components : aliased ASF.Factory.Component_Factory;
      View       : ASF.Views.Facelets.Facelet;
      Ctx        : Facelet_Context;
   begin
      Initialize (Factory, Components'Unchecked_Access, "regtests/files/views;.",
                  True, True, True);
      Find_Facelet (Factory, "text.xhtml", Ctx, View);

      T.Assert (Condition => not Is_Null (View),
                Message   => "Loading an existing facelet should return a view");
   end Test_Load_Facelet;

   --  ------------------------------
   --  Test loading of an unknown file
   --  ------------------------------
   procedure Test_Load_Unknown_Facelet (T : in out Test) is
      Factory    : ASF.Views.Facelets.Facelet_Factory;
      Components : aliased ASF.Factory.Component_Factory;
      View       : ASF.Views.Facelets.Facelet;
      Ctx        : Facelet_Context;
   begin
      Initialize (Factory, Components'Unchecked_Access, "regtests/files;.", True, True, True);
      Find_Facelet (Factory, "not-found-file.xhtml", Ctx, View);

      T.Assert (Condition => Is_Null (View),
                Message   => "Loading a missing facelet should not raise any exception");
   end Test_Load_Unknown_Facelet;

   package Caller is new Util.Test_Caller (Test, "Views.Facelets");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Caller.Add_Test (Suite, "Test ASF.Views.Facelets.Find_Facelet",
                       Test_Load_Facelet'Access);
      Caller.Add_Test (Suite, "Test ASF.Views.Facelets.Find_Facelet",
                       Test_Load_Unknown_Facelet'Access);
   end Add_Tests;

end ASF.Views.Facelets.Tests;
