# Validators
The validators participate in the validation of submitted values during
the request validation phase.  They are responsible for checking whether the
input parameter complies with the validation rules associated with the input
field.  The validator is expected to raise an exception and an error message
is then associated with the faulty input field.

The validator is described in the XHTML file by using one of the following
JSF core components:

| Component              | Validation type                                     |
|------------------------|-----------------------------------------------------|
|  f:validateLength      | The input field must have a minimun/maximum length  |
|  f:validateLongRange   | The input field must be a number in the given range |
|  f:validator           | An Ada registered or custom validator is used       |
|  f:validateRegex       | The input field must match the regular expression   |

A validator instance must implement the `ASF.Validators.Validator` interface.
It only needs to implement the `Validate` procedure which gets the UI
component, the faces request context and the submitted value.  It must verify
the value according to the validator's rule and the UI component.  When the
value is incorrect, it must set an error message in the UI component so that
some user friendly message is reported.  In case of error, it must also
raise the `Invalid_Value` exception.

## Length validator
The `ASF.Validators.Texts.Length_Validator` implements the validator for the
`<f:validateLength>` XHTML validator.

## Regex validator
The `ASF.Validators.Texts.Regex_Validator` implements the validator for the
`<f:validateRegex>` XHTML validator.

## Range validator
The <b>ASF.Validators.Numbers</b> defines various number oriented validators.


