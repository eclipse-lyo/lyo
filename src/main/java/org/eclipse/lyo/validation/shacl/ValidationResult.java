package org.eclipse.lyo.validation.shacl;

import es.weso.schema.ErrorInfo;
import es.weso.schema.Result;
import org.apache.jena.rdf.model.Model;

import java.util.List;

public interface ValidationResult {

    Result getResult();

    void setResult(Result result);

    Boolean isValid();

    void setValid(Boolean valid);

    String getMessage();

    void setMessage(String message);

    Model getValidationReport();

    void setValidationReport(Model validationReport);

    List<ErrorInfo> getErrors();

    void setErrors(List<ErrorInfo> errors);
}
