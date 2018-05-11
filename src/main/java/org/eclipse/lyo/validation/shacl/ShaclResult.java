/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

/**
 * @since 2.3.0
 */

package org.eclipse.lyo.validation.shacl;

import es.weso.schema.ErrorInfo;
import es.weso.schema.Result;
import org.apache.jena.rdf.model.Model;

import java.util.List;
import java.util.Objects;

/**
 * The Class ShaclResult.
 */
public class ShaclResult implements ValidationResult {

    Result result;
    Boolean isValid;
    String message;
    Model validationReport;
    List<ErrorInfo> errors;

    public ShaclResult(Result result, Boolean isValid, String message, Model validationReport, List<ErrorInfo> errors) {
        this.result = result;
        this.isValid = isValid;
        this.message = message;
        this.validationReport = validationReport;
        this.errors = errors;
    }

    public ShaclResult() {
        super();
    }

    @Override
    public Result getResult() {
        return result;
    }

    @Override
    public void setResult(Result result) {
        this.result = result;
    }

    @Override
    public Boolean isValid() {
        return isValid;
    }

    @Override
    public void setValid(Boolean valid) {
        isValid = valid;
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public Model getValidationReport() {
        return validationReport;
    }

    @Override
    public void setValidationReport(Model validationReport) {
        this.validationReport = validationReport;
    }

    @Override
    public List<ErrorInfo> getErrors() {
        return errors;
    }

    @Override
    public void setErrors(List<ErrorInfo> errors) {
        this.errors = errors;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ShaclResult that = (ShaclResult) o;
        return Objects.equals(result, that.result) &&
                Objects.equals(isValid, that.isValid) &&
                Objects.equals(message, that.message) &&
                Objects.equals(validationReport, that.validationReport) &&
                Objects.equals(errors, that.errors);
    }

    @Override
    public int hashCode() {

        return Objects.hash(result, isValid, message, validationReport, errors);
    }

    @Override
    public String toString() {
        return "ValidationResult{" +
                "result=" + result +
                ", isValid=" + isValid +
                ", message='" + message + '\'' +
                ", validationReport=" + validationReport +
                ", errors=" + errors +
                '}';
    }
}
