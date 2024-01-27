

Value
comptime_eval_expression(Type_Context* tcx, Ast* expr, bool report_error=true) {
    
    type_infer_expression(tcx, expr, t_s64, report_error);
    if (!tcx->error_count) {
        Interp interp = {};
        
        Interp_Value result = interp_expression(tcx->interp, expr);
        if (!interp.error_count) {
            return result.value;
        }
    }
    
    
    Value value = {};
    return value;
}