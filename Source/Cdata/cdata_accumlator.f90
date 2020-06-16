module accumulator_routines
    
    use precision_parameters
    
    use setup_data, only: datapath, project, extension
    
    use pp_params, only: mxgenerators, mxpntsarray, mxseeds, mxfields, rnd_seeds, restart_values
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds
    use preprocessor_types, only: random_generator_type
    use preprocessor_output_routines, only: flush_parameters_buffer, setup_col_parameters_output, &
        open_preprocessor_outputfiles, initialize_preprocessor_output_routines, &
        add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer
    
    implicit none
    external cfastexit
    
    private

    public accumulator

    contains
    
    !-------------------------accumulator------------------------------------------------
    
    subroutine accumulator 
    
    end subroutine accumulator
    
end module accumulator_routines